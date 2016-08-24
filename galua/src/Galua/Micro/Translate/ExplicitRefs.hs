-- | Rewrite the staments in a Lua program, to add explicit reference
-- allocation, reference reads, and writes.
module Galua.Micro.Translate.ExplicitRefs (explicitBlocks) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad(liftM,ap)

import qualified Language.Lua.Bytecode as OP
import           Galua.Micro.AST
import           Galua.Micro.Translate.Monad (M,generate,inBlock)
import qualified Galua.Micro.Translate.Monad as M



--------------------------------------------------------------------------------

explicitBlocks :: Map BlockName (Set OP.Reg) ->
                  Map BlockName (Vector BlockStmt) ->
                  Map BlockName (Vector BlockStmt)
explicitBlocks refs blocks = generate $ mapM_ oneBlock $ Map.toList blocks
  where
  oneBlock (name, stmts) =
    let rs = Map.findWithDefault Set.empty name refs
    in inBlock name (runRefs refs rs (mapM_ refStmt (Vector.toList stmts)))




--------------------------------------------------------------------------------

-- This is a state monad for the set of references. The state can
-- change during 'newRef'. In the case of compiling NewClosure it
-- can occur that we need to assign to a reference immediately after
-- creating it.
newtype R a = R (Map BlockName (Set OP.Reg) -> Set OP.Reg -> M (a, Set OP.Reg))

instance Functor R where
  fmap  = liftM

instance Applicative R where
  pure = doM . pure
  (<*>) = ap

instance Monad R where
  R m >>= f = R (\rs refs -> do (a,refs1) <- m rs refs
                                let R m1 = f a
                                m1 rs refs1)

reader :: (Set OP.Reg -> M a) -> R a
reader f = R $ \_ refs -> do x <- f refs
                             return (x,refs)

isRef :: Reg -> R Bool
isRef r = case r of
            Reg r' -> reader (\refs -> return (r' `Set.member` refs))
            _      -> return False

runRefs :: Map BlockName (Set OP.Reg) -> Set OP.Reg -> R a -> M a
runRefs allRefs refs (R f) = fmap fst (f allRefs refs)

doM :: M a -> R a
doM m = reader (const m)

ifRef :: Reg -> R a -> R a -> R a
ifRef r m1 m2 =
  do yes <- isRef r
     if yes then m1 else m2

emit :: Stmt -> R ()
emit s = doM (M.emit s)

newTMP :: R Reg
newTMP = doM (M.newPhaseTMP 2)

newRef :: IsExpr e => Reg -> e -> R ()
newRef r e = R $ \_ refs ->
  do M.emit (NewRef r (toExpr e))
     let refs' = case r of
                   Reg reg -> Set.insert reg refs
                   TMP{}   -> error "newRef: TMP"
     return ((), refs')

readRef :: IsExpr e => Reg -> e -> R ()
readRef r e = emit (ReadRef r (toExpr e))

writeRef :: (IsExpr e1, IsExpr e2) => e1 -> e2 -> R ()
writeRef e1 e2 = emit (WriteRef (toExpr e1) (toExpr e2))

endBlock :: BlockName -> R BlockName
endBlock tgt =
  do newRefs <- shouldBeRefs
     if Set.null newRefs
       then return tgt
       else doM $ M.inNewBlock_ $
               do let toRef r = M.emit (NewRef (Reg r) (toExpr r))
                  mapM_ toRef newRefs
                  M.emit (Goto tgt)
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

refStmt :: BlockStmt -> R ()
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

    Raise e ->
      do e' <- readExpr e
         emit $ Raise e'


    Goto l -> do l' <- endBlock l
                 emit (Goto l')

    Case e as d ->
      do e' <- readExpr e
         let alt (v,b) = do b' <- endBlock b
                            return (v,b')
         as' <- mapM alt as
         d'  <- mapM endBlock d
         emit $ Case e' as' d'

    If p t f ->
      do p' <- readProp p
         t' <- endBlock t
         f' <- endBlock f
         emit $ If p' t' f'



    Call f ->
      do f' <- readReg f
         emit $ Call f'

    TailCall f ->
      do f'    <- readReg f
         emit $ TailCall f'

    Return  ->
      do emit Return

    Drop list n ->
      do emit $ Drop list n

    Append list xs ->
      do xs' <- traverse readExpr xs
         emit $ Append list xs'

    SetList list xs ->
      do xs' <- traverse readExpr xs
         emit $ SetList list xs'

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
    NewClosure r c us ->
      do mapM_ upVal us
         setReg r $ \r' -> NewClosure r' c us
      where
      upVal u =
        case u of
          EUp _   -> return ()
          EReg r' -> ifRef r' (return ()) (newRef r' u)
          ELit _  -> error "upVal: ELit"

    NewRef _ _    -> error "NewRef: wrong phase"
    ReadRef _ _   -> error "ReadRef: wrong phase"
    WriteRef _ _  -> error "WriteRef: wrong phase"



