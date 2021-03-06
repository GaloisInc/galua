-- | Rewrite the staments in a Lua program, to add explicit reference
-- allocation, reference reads, and writes.
module Galua.Micro.Translate.ExplicitRefs (explicitBlocks) where

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad(liftM,ap)

import qualified Galua.Code as Code
import           Galua.Micro.AST
import           Galua.Micro.Translate.Monad (M,generate,inBlock)
import qualified Galua.Micro.Translate.Monad as M



--------------------------------------------------------------------------------

explicitBlocks :: Map BlockName (Set Code.Reg) ->
                  Map BlockName Block ->
                  Map BlockName Block
explicitBlocks refs blocks = generate $ mapM_ oneBlock $ Map.toList blocks
  where
  oneBlock (name, stmts) =
    let rs = Map.findWithDefault Set.empty name refs
    in inBlock name (runRefs refs rs (doBlock stmts))

  doBlock b = do mapM_ refStmt (Vector.toList (blockBody b))
                 refEndStmt (blockEnd b)



--------------------------------------------------------------------------------

-- This is a state monad for the set of references. The state can
-- change during 'newRef'. In the case of compiling NewClosure it
-- can occur that we need to assign to a reference immediately after
-- creating it.
newtype R a = R (Map BlockName (Set Code.Reg) ->
                Set Code.Reg ->
                M (a, Set Code.Reg))

instance Functor R where
  fmap  = liftM

instance Applicative R where
  pure = doM . pure
  (<*>) = ap

instance Monad R where
  R m >>= f = R (\rs refs -> do (a,refs1) <- m rs refs
                                let R m1 = f a
                                m1 rs refs1)

reader :: (Set Code.Reg -> M a) -> R a
reader f = R $ \_ refs -> do x <- f refs
                             return (x,refs)

isRef :: Reg -> R Bool
isRef r = case r of
            Reg r' -> reader (\refs -> return (r' `Set.member` refs))
            _      -> return False

runRefs :: Map BlockName (Set Code.Reg) -> Set Code.Reg -> R a -> M a
runRefs allRefs refs (R f) = fmap fst (f allRefs refs)

doM :: M a -> R a
doM m = reader (const m)

ifRef :: Reg -> R a -> R a -> R a
ifRef r m1 m2 =
  do yes <- isRef r
     if yes then m1 else m2

emit :: Stmt -> R ()
emit s = doM (M.emit s)

emitEnd :: EndStmt -> R ()
emitEnd s = doM (M.emitEnd s)

newTMP :: R Reg
newTMP = doM (M.newPhaseTMP 2)

newRef :: IsExpr e => Reg -> e -> R ()
newRef r' e = R $ \_ refs ->
  do let (r,refs') = case r' of
                       Reg cr  -> (Ref cr, Set.insert cr refs)
                       Ref {} -> (r', refs)
                       TMP {} -> error "newRef: result in TMP"
     M.emit (NewRef r (toExpr e))
     return ((), refs')

class IsRef t where
  toRefExpr :: t -> Expr

instance IsRef Reg where
  toRefExpr r = case r of
                  Reg cr -> EReg (Ref cr)
                  TMP {} -> error "toRefExpr: TMP"
                  Ref {} -> EReg r

instance IsRef Expr where
  toRefExpr expr =
    case expr of
      EReg r  -> toRefExpr r
      ELit {} -> error "toRefExpr: ELit"
      EUp {}  -> expr

readRef :: IsRef e => Reg -> e -> R ()
readRef r e = emit (ReadRef r (toRefExpr e))

writeRef :: (IsRef e1, IsExpr e2) => e1 -> e2 -> R ()
writeRef e1 e2 = emit (WriteRef (toRefExpr e1) (toExpr e2))

endBlock :: BlockName -> R BlockName
endBlock tgt =
  do newRefs <- shouldBeRefs
     if Set.null newRefs
       then return tgt
       else doM $ M.inNewBlock_ $
               do let toRef r = M.emit (NewRef (Ref r) (toExpr r))
                  mapM_ toRef newRefs
                  M.emitEnd (Goto tgt)
  where
  shouldBeRefs =
    R $ \allRs nowRs ->
         return ( Set.difference (Map.findWithDefault Set.empty tgt allRs) nowRs
                , nowRs
                )



--------------------------------------------------------------------------------



readReg :: Reg -> R Reg
readReg reg =
  ifRef reg (do tmp <- newTMP
                readRef tmp reg
                return tmp)
            (return reg)

setReg :: Reg -> (Reg -> Stmt) -> R ()
setReg r stmt =
  ifRef r (do tmp <- newTMP
              emit (stmt tmp)
              writeRef r tmp)
          (emit (stmt r))

readExpr :: Expr -> R Expr
readExpr expr =
  case expr of
    EReg r -> EReg <$> readReg r
    EUp u  -> do tmp <- newTMP
                 readRef tmp (EUp u)
                 return (EReg tmp)
    ELit l -> return (ELit l)

readProp :: Prop -> R Prop
readProp (Prop p es) = Prop p <$> mapM readExpr es

refEndStmt :: BlockStmt EndStmt -> R ()
refEndStmt stmt =
  case stmtCode stmt of
    Raise e ->
      do e' <- readExpr e
         emitEnd $ Raise e'


    Goto l -> do l' <- endBlock l
                 emitEnd (Goto l')

    Case e as d ->
      do e' <- readExpr e
         let alt (v,b) = do b' <- endBlock b
                            return (v,b')
         as' <- mapM alt as
         d'  <- mapM endBlock d
         emitEnd $ Case e' as' d'

    If p t f ->
      do p' <- readProp p
         t' <- endBlock t
         f' <- endBlock f
         emitEnd $ If p' t' f'

    TailCall f ->
      do f'    <- readReg f
         emitEnd $ TailCall f'

    Return  ->
      do emitEnd Return




refStmt :: BlockStmt Stmt -> R ()
refStmt stmt =
  case stmtCode stmt of

    Assign r e ->
      do e' <- readExpr e
         ifRef r (writeRef r e') (emit (Assign r e'))

    SetUpVal ix r ->
      do r' <- readReg r
         writeRef (EUp ix) r'

    NewTable r -> setReg r $ \r' -> NewTable r'

    LookupTable r tab ix ->
      do ix'  <- readExpr ix
         tab' <- readReg  tab
         setReg r $ \r' -> LookupTable r' tab' ix'

    SetTable tab ix val ->
      do tab' <- readReg tab
         ix'  <- readExpr ix
         val' <- readExpr val
         emit $ SetTable tab' ix' val'

    SetTableList tab ix ->
      do setReg tab $ \tab' -> SetTableList tab' ix

    GetMeta r e ->
      do e' <- readExpr e
         setReg r $ \r' -> GetMeta r' e'


    Call f ->
      do f' <- readReg f
         emit $ Call f'

    Drop list n ->
      do emit $ Drop list n

    Append list xs ->
      do xs' <- traverse readExpr xs
         emit $ Append list xs'

    SetList list xs ->
      do xs' <- traverse readExpr xs
         emit $ SetList list xs'

    AssignListReg xs ys ->
         emit $ AssignListReg xs ys

    IndexList r list ix ->
      do setReg r $ \r' -> IndexList r' list ix

    Arith1 r op e1 ->
      do e1' <- readExpr e1
         setReg r $ \r' -> Arith1 r' op e1'

    Arith2 r op e1 e2 ->
      do e2' <- readExpr e2
         e1' <- readExpr e1
         setReg r $ \r' -> Arith2 r' op e1' e2'

    Comment x -> emit (Comment x)

    CloseStack _ -> return ()   -- It was there only for analysis
    NewClosure r c f ->
      do let us = funcUpvalExprs f
         mapM_ upVal us
         setReg r $ \r' -> NewClosure r' c f
      where
      upVal u =
        case u of
          EUp _   -> return ()
          EReg r' -> ifRef r' (return ()) (newRef r' u')
            where u' = if r' == r then ELit LNil else u
          ELit _  -> error "upVal: ELit"

    NewRef _ _    -> error "NewRef: wrong phase"
    ReadRef _ _   -> error "ReadRef: wrong phase"
    WriteRef _ _  -> error "WriteRef: wrong phase"



