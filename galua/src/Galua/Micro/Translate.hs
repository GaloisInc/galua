{-# LANGUAGE OverloadedStrings #-}
-- | Translate from Lua op-codes into the CFG representation of a program.
module Galua.Micro.Translate
  ( MicroFunction(..)
  , translate,translateAll,translateTop
  ) where

import           Data.String(fromString)
import           Data.ByteString(ByteString)
import           Data.Foldable
import           Control.Monad
import qualified Data.Vector as Vector
import           Data.Map(Map)
import qualified Data.Map as Map
import           Control.Monad.Fix(mfix)

import qualified Galua.Code as Code
import           Galua.Code (FunId, subFun, rootFun)
import           Galua.Micro.AST
import           Galua.Micro.Translate.Monad
import           Galua.Micro.Translate.AnalyzeRefs(analyze)
import           Galua.Micro.Translate.ExplicitRefs(explicitBlocks)
import           Galua.Micro.Translate.Simplify(simplify)
import           Galua.Micro.Translate.JoinBlocks(joinBlocks)
import           Galua.Micro.Translate.RenumberTMP(renumberTMP)

translate :: Code.Function -> MicroFunction
translate fun = renumberTMP lastPhase
              $ joinBlocks
              $ simplify
              $ pass1 { functionCode = explicitBlocks refs code1 }
  where
  pass1 = translatePass1 fun
  code1 = functionCode pass1
  refs  = analyze (Code.Reg (Code.funcMaxStackSize fun - 1)) code1

translateAll :: FunId -> Code.Function -> Map FunId MicroFunction ->
                                                        Map FunId MicroFunction
translateAll cur f m0 =
  go m0 (zip [ 0 .. ] (Vector.toList (Code.funcNested f)))
  where
  go mp ((n,sub) : rest) = go (translateAll (subFun cur n) sub mp) rest
  go mp []               = Map.insert cur (translate f) mp

translateTop :: Int -> Code.Function -> Map FunId MicroFunction
translateTop n f = translateAll (rootFun n) f Map.empty


--------------------------------------------------------------------------------

lastPhase :: Int
lastPhase = 3

newTMP :: M Reg
newTMP = newPhaseTMP 1

translatePass1 :: Code.Function -> MicroFunction
translatePass1 fun =
  MicroFunction
    { functionCode = generate $
                do prelude (Code.funcNumParams fun)
                   mapM_ (micro fun) $ take (Vector.length code) [ 0 .. ]
    , functionRegsTMP = 0
    }
  where
  code = Code.funcCode fun

prelude :: Int -> M ()
prelude params =
  inBlock EntryBlock $
    do for_ (take params [0..]) $ \i ->
           do emit (IndexList (Reg (Code.Reg i)) ArgReg i)
       when (params > 0) (emit (Drop ArgReg params))
       goto (PCBlock 0)

micro :: Code.Function -> Int -> M ()
micro fun pc =

  inBlock (PCBlock pc) $
    case getOpCode fun pc of

      -- Setting registers
      Code.OP_MOVE tgt src ->
        do Reg tgt =: src
           advance

      Code.OP_LOADK tgt k ->
        do Reg tgt =: k
           advance

      Code.OP_LOADKX tgt k ->
        do Reg tgt =: k
           jump 1

      Code.OP_LOADBOOL tgt b withJump ->
        do Reg tgt =: b
           jump (if withJump then 1 else 0)

      Code.OP_LOADNIL tgt count ->
        do let mkNil r = Reg r =: LNil
           mapM_ mkNil $ Code.regRange tgt (count + 1)
           advance


      -- Up values
      Code.OP_GETUPVAL tgt src ->
        do Reg tgt =: src
           advance

      Code.OP_SETUPVAL src tgt ->
        do emit $ SetUpVal tgt (Reg src)
           advance

      Code.OP_GETTABUP tgt src tabKey ->
        indexValue (Reg tgt) src (fromRK tabKey) (PCBlock (pc + 1))

      Code.OP_SETTABUP tab tabKey val ->
        setTable tab (fromRK tabKey) (fromRK val) (PCBlock (pc + 1))



      -- Tables
      Code.OP_NEWTABLE tgt _ _ ->
        do emit $ NewTable (Reg tgt)
           advance

      Code.OP_GETTABLE tgt src tabKey ->
        indexValue (Reg tgt) src (fromRK tabKey) (PCBlock (pc + 1))

      Code.OP_SETTABLE tab tabKey val ->
        setTable tab (fromRK tabKey) (fromRK val) (PCBlock (pc + 1))

      Code.OP_SELF tgt tab key ->
        do tmp <- newTMP
           tmp =: tab
           indexValue (Reg tgt) tab (fromRK key) $
             do Reg (Code.plusReg tgt 1) =: tmp
                advance


      -- Arithmethic
      Code.OP_ADD  tgt op1 op2 -> binArithOp DoAdd tgt op1 op2
      Code.OP_SUB  tgt op1 op2 -> binArithOp DoSub tgt op1 op2
      Code.OP_MUL  tgt op1 op2 -> binArithOp DoMul tgt op1 op2

      Code.OP_POW  tgt op1 op2 -> binArithOp DoPow tgt op1 op2
      Code.OP_DIV  tgt op1 op2 -> binArithOp DoDiv tgt op1 op2
      Code.OP_MOD  tgt op1 op2 -> binArithOp DoMod tgt op1 op2

      Code.OP_IDIV tgt op1 op2 -> binArithOp DoIDiv tgt op1 op2
      Code.OP_BAND tgt op1 op2 -> binArithOp DoAnd  tgt op1 op2
      Code.OP_BOR  tgt op1 op2 -> binArithOp DoOr   tgt op1 op2
      Code.OP_BXOR tgt op1 op2 -> binArithOp DoXor  tgt op1 op2
      Code.OP_SHL  tgt op1 op2 -> binArithOp DoShl  tgt op1 op2
      Code.OP_SHR  tgt op1 op2 -> binArithOp DoShr  tgt op1 op2

      Code.OP_UNM tgt op1      -> unArithOp DoUnaryMinus tgt op1
      Code.OP_BNOT tgt op1     -> unArithOp DoComplement tgt op1

      Code.OP_LEN tgt op -> valueLength (Reg tgt) (Reg op) (PCBlock (pc + 1))

      Code.OP_CONCAT tgt start end ->
        valueConcat (Reg tgt) (map toExpr (Code.regFromTo start end))
                              (PCBlock (pc + 1))

      Code.OP_NOT tgt op ->
        valueIf op (do Reg tgt =: False
                       advance)
                   (do Reg tgt =: True
                       advance)

      -- Relational operators
      Code.OP_EQ invert op1 op2 -> relOp valueEqual invert op1 op2
      Code.OP_LT invert op1 op2 -> relOp (valueLess True)  invert op1 op2
      Code.OP_LE invert op1 op2 -> relOp (valueLess False) invert op1 op2
      Code.OP_TEST ra c -> valueIf ra (PCBlock (pc + yes)) (PCBlock (pc + no))
        where (yes,no) = if c then (1,2) else (2,1)

      Code.OP_TESTSET ra rb c ->
        do let upd = do Reg ra =: c
                        advance

               skip = PCBlock (pc + 1)

           if c then valueIf rb upd skip
                else valueIf rb skip upd

      -- Function calls
      Code.OP_CALL a b c ->
        do let a' = Code.plusReg a 1
           getCallArguments a' b
           callValue (Reg a) $
             do setCallResults a ListReg c
                advance

      Code.OP_TAILCALL a b _c ->
        do let a' = Code.plusReg a 1
           getCallArguments a' b
           resolveFunction (Reg a) $
             emitEnd (TailCall (Reg a))

      Code.OP_RETURN a c ->
        do getCallArguments a c
           emitEnd Return



      Code.OP_JMP mbCloseReg jmp ->
        do mapM_ (emit . CloseStack . Reg) mbCloseReg
           jump jmp

      Code.OP_CLOSURE tgt i f ->
        do emit (NewClosure (Reg tgt) i f)
           advance

      -- Loops
      Code.OP_FORPREP a jmp ->
        forloopAsNumber     a             $ \initial ->
          forloopAsNumber   (Code.plusReg a 1) $ \limit ->
            forloopAsNumber (Code.plusReg a 2) $ \step ->
              implementationForArith2 DoSub (Reg a) initial step $
                do Reg (Code.plusReg a 1) =: limit
                   Reg (Code.plusReg a 2) =: step
                   jump jmp


      Code.OP_FORLOOP a sBx ->
        do let initial = Reg a
               limit   = Reg (Code.plusReg a 1)
               step    = Reg (Code.plusReg a 2)

           next <- newTMP
           let check x y =
                 ite (Prop NumberLEQ [toExpr x, toExpr y])
                    (do Reg a             =: next
                        Reg (Code.plusReg a 3) =: next
                        jump sBx)
                    (PCBlock (pc + 1))

           implementationForArith2 DoAdd next initial step $
             ite (Prop NumberLT [zero, toExpr step])
                 (check next limit)
                 (check limit next)

      Code.OP_TFORLOOP a sBx ->
        ifNil (Code.plusReg a 1) (PCBlock (pc + 1))
                       (do Reg a =: Code.plusReg a 1
                           jump sBx)

      Code.OP_TFORCALL a c ->
        do let arg n = toExpr (Code.plusReg a n)
           emit (SetList ListReg [ arg 1, arg 2 ])
           callValue (Reg a) $
             do let results = zip (Code.regRange (Code.plusReg a 3) c) [ 0 .. ]
                mapM_ emit [ IndexList (Reg r) ListReg n | (r,n) <- results ]
                advance


      -- Arguments

      Code.OP_SETLIST a b offset skip ->
        typeCase a
           $ IfType TableType
             (do let a' = Code.plusReg a 1
                 case b of
                   Code.CountTop ->
                      do mb <- getListReg
                         case mb of
                           Just r
                             | r >= a' ->
                               do emit $ Append ListReg
                                       $ map toExpr
                                       $ init $ Code.regFromTo a' r
                                  emit $ SetTableList (Reg a) (1 + offset)
                             | otherwise -> error "OP_SETLIST: r < a'"
                           Nothing -> error "OP_SETLIST: missing list register"
                   Code.CountInt j ->
                      mapM_ emit [ SetTable (Reg a) (toExpr (offset + i))
                                                    (toExpr (Code.plusReg a i))
                                      | i <- [ 1 .. j ] ]

                 jump skip
            )
            NoDefault


      Code.OP_VARARG a b ->
        do setCallResults a ArgReg b
           advance


      Code.OP_EXTRAARG{} -> return ()



  where
  unArithOp op tgt op1 =
    valueArith1 op (Reg tgt) op1 (PCBlock (pc + 1))

  binArithOp op tgt op1 op2 =
    valueArith2 op (Reg tgt) (fromRK op1) (fromRK op2) (PCBlock (pc + 1))

  relOp op invert op1 op2 =
    op (fromRK op1) (fromRK op2) (PCBlock (pc + yes)) (PCBlock (pc + no))
    where (yes,no) = if invert then (1,2) else (2,1)


  jump x  = goto (PCBlock (pc + 1 + x))
  advance = jump 0

  fromRK rk =
    case rk of
      Code.RK_Reg r -> toExpr r
      Code.RK_Kst k -> toExpr k


--------------------------------------------------------------------------------

getOpCode :: Code.Function -> Int -> Code.OpCode
getOpCode fun pc =
  case Code.funcCode fun Vector.!? pc of
    Just op -> op
    Nothing -> error ("Bad PC: " ++ show pc)

--------------------------------------------------------------------------------
-- Tables

indexValue ::
  (ToBlockName k, IsExpr e1, IsExpr e2) => Reg -> e1 -> e2 -> k -> M ()
indexValue ans tab0 key' fin =
  do done <- toBlockName fin
     tab  <- newTMP
     tab =: tab0
     goto =<< mfix (block tab done)

  where
  key = toExpr key'

  block tab done start =
    do let meta :: M () -> M ()
           meta next =
             valueMetamethod "__index" tab
               (\meth ->
                  typeCase meth
                    $ IfType FunctionType
                        (do emit $ SetList ListReg [ toExpr tab, toExpr key ]
                            emit $ Call meth
                            emit $ IndexList ans ListReg 0
                            goto done
                        )
                    $ Default
                        (do tab =: meth
                            goto start)
                )

               next


       inNewBlock_ $
         typeCase tab
           $ IfType TableType
               (do tmp <- newTMP
                   emit (LookupTable tmp tab key)
                   ifNil tmp (meta $ do ans =: LNil
                                        goto done)
                             (do ans =: tmp
                                 goto done)
               )
           $ Default
               (raiseError "Bad index")


setTable :: IsExpr tab => tab -> Expr -> Expr -> BlockName -> M ()
setTable tab0 key val k =
  do tab <- newTMP
     tab =: tab0
     goto =<< mfix (block tab)


  where
  block tab start =
    do let meta :: M () -> M ()
           meta onFailure =
             valueMetamethod "__newindex" tab
                (\meth ->
                   typeCase meth
                     $ IfType FunctionType
                          (do emit $ SetList ListReg [ toExpr tab, key, val ]
                              emit $ Call meth
                              goto k
                          ) $
                       Default
                         (do tab =: meth
                             goto start)
                )
                onFailure

       inNewBlock_ $
          typeCase tab
            $ IfType TableType
                ( do tmp <- newTMP
                     emit $ LookupTable tmp tab key

                     keyOk <- inNewBlock_ $ do emit (SetTable tab key val)
                                               goto k

                     rawSet <- inNewBlock_ $
                        typeCase key
                          $ IfType NilType (raiseError "setTable: nil key")
                          $ IfType NumberType
                              (ite (Prop IsNaN [key])
                                   (raiseError "setTable: NaN key")
                                   keyOk)
                          $ Default keyOk

                     ifNil tmp (meta (goto rawSet)) rawSet
                )
            $ Default
                (meta $ raiseError "setTable: not a table")


--------------------------------------------------------------------------------
-- Overloading

valueMetamethod :: IsExpr e => ByteString -> e -> (Reg -> M ()) -> M () -> M ()
valueMetamethod event tab ifOK ifFail =

{- Version that disables metatables.
  do failCase <- inNewBlock_ ifFail
     emitEnd (Goto failCase)
-}

  do meta  <- newTMP
     emit (GetMeta meta (toExpr tab))

     failCase <- inNewBlock_ ifFail

     typeCase meta
       $ IfType NilType failCase
       $ IfType TableType
          (do emit (LookupTable meta meta (toExpr event))
              ifNil meta failCase (ifOK meta)
          )
       $ NoDefault -}

valueMetamethod2 :: ByteString -> Expr -> Expr -> (Reg -> M ()) -> M () -> M ()
valueMetamethod2 event left right ifOk ifFail =
  valueMetamethod event left ifOk (valueMetamethod event right ifOk ifFail)



--------------------------------------------------------------------------------

data DoArith1 = DoComplement | DoUnaryMinus

eventForArith1 :: DoArith1 -> ByteString
eventForArith1 op =
  case op of
    DoUnaryMinus  -> "__unm"
    DoComplement  -> "__bnot"

convertForArith1 :: DoArith1 -> Op1
convertForArith1 op =
  case op of
    DoUnaryMinus  -> ToNumber
    DoComplement  -> ToInt

implementationForArith1 ::
  (ToBlockName k, IsExpr e) => DoArith1 -> Reg -> e -> k -> M ()
implementationForArith1 doWhat ans e k =
  do next <- toBlockName k
     case doWhat of
       DoComplement -> do arith1 ans Complement e
                          goto next
       DoUnaryMinus -> do arith1 ans NumberUnaryMinus e
                          goto next


valueArith1 :: (IsExpr e, ToBlockName k) => DoArith1 -> Reg -> e -> k -> M ()
valueArith1 doWhat ans x fin =
  do done <- toBlockName fin
     num  <- newTMP
     let convert = convertForArith1 doWhat
     arith1 num convert x

     typeCase num
        $ IfType NilType
            (valueMetamethod (eventForArith1 doWhat) x
                 (\meth -> do emit $ SetList ListReg [ toExpr num ]
                              callValue meth $
                                do emit $ IndexList ans ListReg 0
                                   goto done
                 )
                 (raiseError "Bad arithmetic 1")
            )
        $ IfType NumberType
            (implementationForArith1 doWhat ans num done)
        $ NoDefault





--------------------------------------------------------------------------------

data DoArith2 = DoAdd | DoSub | DoMul
              | DoPow | DoMod | DoDiv | DoIDiv
              | DoAnd | DoOr  | DoXor
              | DoShl | DoShr

convertForArith2 :: DoArith2 -> Op1
convertForArith2 op =
  case op of
    DoAdd   -> ToNumber
    DoSub   -> ToNumber
    DoMul   -> ToNumber

    DoPow   -> ToNumber
    DoMod   -> ToNumber
    DoDiv   -> ToNumber

    DoIDiv  -> ToInt
    DoAnd   -> ToInt
    DoOr    -> ToInt
    DoXor   -> ToInt
    DoShl   -> ToInt
    DoShr   -> ToInt

eventForArith2 :: DoArith2 -> ByteString
eventForArith2 op =
  case op of
    DoAdd   -> "__add"
    DoSub   -> "__sub"
    DoMul   -> "__mul"

    DoPow   -> "__pow"
    DoMod   -> "__mod"
    DoDiv   -> "__div"
    DoIDiv  -> "__idiv"

    DoAnd   -> "__band"
    DoOr    -> "__bor"
    DoXor   -> "__bxor"
    DoShl   -> "__shl"
    DoShr   -> "__shr"

implementationForArith2 ::
  (IsExpr e1, IsExpr e2, ToBlockName k) =>
    DoArith2 -> Reg -> e1 -> e2 -> k -> M ()
implementationForArith2 doWhat r e1 e2 k =
  do next <- toBlockName k

     let singleMode op = do arith2 r op e1 e2
                            goto next

         dualMode :: M () -> Op2 -> M ()
         dualMode intVer fOp =
           let e1' = toExpr e1
               e2' = toExpr e2'

           in ite (Prop IsInteger [e1'])
                    (ite (Prop IsInteger [e2'])
                         intVer
                         (do x1 <- newTMP
                             arith1 x1 IntToDouble e1'
                             arith2 r fOp x1 e2'
                             goto next
                         )
                    )
                    (ite (Prop IsInteger [e2'])
                         (do x2 <- newTMP
                             arith1 x2 IntToDouble e2'
                             arith2 r fOp e1' x2
                             goto next
                         )
                         (do arith2 r fOp e1' e2'
                             goto next
                         )
                    )

     case doWhat of
       DoAdd  -> singleMode NumberAdd
       DoSub  -> singleMode NumberSub
       DoMul  -> singleMode NumberMul
       DoMod  -> dualMode (zeroCheck IMod r e1 e2 next) FMod

       DoPow  -> singleMode NumberPow
       DoDiv  -> singleMode NumberDiv

       DoIDiv -> zeroCheck IDiv r e1 e2 next

       DoAnd  -> singleMode And
       DoOr   -> singleMode Or
       DoXor  -> singleMode Xor
       DoShl  -> singleMode Shl
       DoShr  -> singleMode Shr





valueArith2 :: ToBlockName k => DoArith2 -> Reg -> Expr -> Expr -> k -> M ()
valueArith2 doWhat ans left right fin =
  do next <- toBlockName fin

     let convert = convertForArith2 doWhat
     tmp1 <- newTMP
     arith1 tmp1 convert left

     useOverloading <- inNewBlock_ $
       valueMetamethod2 (eventForArith2 doWhat) left right
         (\meth -> do emit (SetList ListReg [ left, right ])
                      callValue meth $
                        do emit $ IndexList ans ListReg 0
                           goto next
         )
         (raiseError "Bad arithmetic")

     ifNil tmp1
       useOverloading
       (do tmp2 <- newTMP
           arith1 tmp2 convert right
           ifNil tmp2
             useOverloading
             (implementationForArith2 doWhat ans tmp1 tmp2 next)
       )



zeroCheck ::
  (IsExpr e1, IsExpr e2) => Op2 -> Reg -> e1 -> e2 -> BlockName -> M ()
zeroCheck op r e1 e2 next =
  ite (Prop Equals [ toExpr e2,  zero ])
      (raiseError $ fromString $ show op ++ " 0")
      (do arith2 r op e1 e2
          goto next
      )




--------------------------------------------------------------------------------
-- Functions


{- | Given a function-like value and an argument list, resolve
it to a real function.  Note that both the function and arfument
registers are over-written. -}
resolveFunction :: Reg -> M () -> M ()
resolveFunction fun fin =
  do goto =<< mfix block

  where
  block start =
    inNewBlock_ $
      typeCase fun
        $ IfType FunctionType fin
        $ Default
           (valueMetamethod "__call" fun
              (\meth -> do emit $ Append ListReg [toExpr fun]
                           fun =: meth
                           goto start
              )
              (raiseError "attempt to call a non-function"))


-- | Call a function-like thing.  Note that all registers are clobbered!
callValue :: Reg -> M () -> M ()
callValue f fin =
  resolveFunction f $
    do emit $ Call f
       fin

getCallArguments :: Code.Reg {- ^ start -} -> Code.Count  {- ^ count -} -> M ()
getCallArguments from count =
  case count of
    Code.CountInt x ->
      emit $ SetList ListReg [ toExpr r | r <- Code.regRange from x ]

    Code.CountTop ->
      do mb <- getListReg
         case mb of
           Just end
             | from <= end ->
                emit $ Append ListReg
                              -- XXX: pred 0?
                          [ toExpr r | r <- init (Code.regFromTo from end) ]

             | otherwise -> error "getCallArguments: from > end"

           Nothing -> error "getCallArguments: CountTop with no list register"



setCallResults ::
  Code.Reg    {- ^ starting register -} ->
  ListReg     {- ^ source register -} ->
  Code.Count  {- ^ results expected  -} ->
  M ()
setCallResults to from count =
  case count of
    Code.CountInt x ->
      mapM_ emit [ IndexList (Reg (Code.plusReg to i)) from i
                      | i <- take x [ 0 .. ]
                 ]
    Code.CountTop ->
      setListReg to



--------------------------------------------------------------------------------
-- Booleans



valueIf :: (IsExpr e, ToBlockName t, ToBlockName f) => e -> t -> f -> M ()
valueIf e ifTrue ifFalse =
  typeCase e
    $ IfType NilType  ifFalse
    $ IfType BoolType (valueIfBool e ifTrue ifFalse)
    $ Default ifTrue

valueIfBool :: (IsExpr e, ToBlockName t, ToBlockName f) => e -> t -> f -> M ()
valueIfBool e = ite (Prop Equals [ toExpr e, toExpr True ])



valueEqual :: Expr -> Expr -> BlockName -> BlockName -> M ()
valueEqual e1 e2 yes no =
  do overload <- inNewBlock_ $
       valueMetamethod2 "__eq" e1 e2
         (\meth -> do res  <- newTMP
                      emit $ SetList ListReg [e1,e2]
                      callValue meth $
                        do emit $ IndexList res ListReg 0
                           valueIf res yes no
         )
         (goto no)

     ite (Prop Equals [ toExpr e1, toExpr e2 ])
         yes
         (typeCase e1
            $ IfType TableType
                ( typeCase e2
                    $ IfType TableType overload
                    $ Default no
                )
            $ IfType UserDataType
                ( typeCase e2
                  $ IfType UserDataType overload
                  $ Default no
                )
            $ Default no
         )




valueLess :: Bool -> Expr -> Expr -> BlockName -> BlockName -> M ()
valueLess strict e1 e2 yes no =
  do overload <- inNewBlock_ $
                   valueMetamethod2 event e1 e2
                     (\meth -> do emit $ SetList ListReg [e1,e2]
                                  res  <- newTMP
                                  callValue meth $
                                    do emit $ IndexList res ListReg 0
                                       valueIf res yes no
                     )
                     (raiseError "Bad compare")

     typeCase e1
       $ IfType NumberType
           (typeCase e2
              $ IfType NumberType (basic NumberType)
              $ Default overload)
       $ IfType StringType
           (typeCase e2
              $ IfType StringType (basic StringType)
              $ Default overload)
       $ Default overload

  where
  basic t = let op = case t of
                       NumberType -> if strict then NumberLT else NumberLEQ
                       StringType -> if strict then StringLT else StringLEQ
                       _          -> error "Bad basic compare"
            in ite (Prop op [e1,e2]) yes no

  event = if strict then "__lt" else "__le"



--------------------------------------------------------------------------------

-- NOTE assumes that the result and the expression are distinct.
forloopAsNumber :: IsExpr e => e -> (Reg -> M ()) -> M ()
forloopAsNumber e k =
  do r <- newTMP
     arith1 r ToNumber e
     typeCase r
       $ IfType NumberType (k r)
       $ IfType NilType
          (raiseError "'for' argument must be convertible to a number")
       $ NoDefault


--------------------------------------------------------------------------------

valueLength :: Reg -> Reg -> BlockName -> M ()
valueLength tgt op next =
  typeCase op
    $ IfType StringType
        (do arith1 tgt StringLen op
            goto next)
    $ Default
        (valueMetamethod "__len" op
           (\meth -> do emit $ SetList ListReg [toExpr op]
                        callValue meth $
                           do emit $ IndexList tgt ListReg 0
                              goto next
           )
           (typeCase op
              $ IfType TableType (do arith1 tgt TableLen op
                                     goto next)
              $ Default          (raiseError "Bad length")
           )
        )

--------------------------------------------------------------------------------


valueConcat :: Reg -> [Expr] -> BlockName -> M ()
valueConcat res es done = concat2 $ reverse es
  where
  concat2 ss =
    case ss of
      []  -> error "Concat 0 things"
      [_] -> error "Concat 1 thing"

      x : y : z ->
        do overload <- inNewBlock_ $
                         valueMetamethod2 "__concat" y x
                            (\meth -> do emit $ SetList ListReg [y,x]
                                         case z of
                                           [] -> callValue meth $
                                                    goto done
                                           _ -> do r <- newTMP
                                                   callValue meth $
                                                     do emit $ IndexList r ListReg 0
                                                        concat2 (toExpr r : z)
                            )
                            (raiseError "Bad concat")

           stringLikePenUlt <- inNewBlock_ $
                                 do ult <- newTMP
                                    arith1 ult ToString x
                                    ifNil ult
                                      overload $
                                      do penult <- newTMP
                                         arith1 penult ToString y
                                         -- we've already tested that this
                                         -- will not return Nil!
                                         case z of
                                           [] -> do arith2 res Concat y x
                                                    goto done
                                           _  -> do r <- newTMP
                                                    arith2 r Concat y x
                                                    concat2 (toExpr r : z)

           typeCase y
             $ IfType StringType stringLikePenUlt
             $ IfType NumberType stringLikePenUlt
             $ Default overload
