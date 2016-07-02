{-# LANGUAGE RecordWildCards, OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
module Galua.Micro.Type.Import (importClosure) where

import           Data.Text(Text)
import qualified Data.Vector as Vector
import           Data.IORef(IORef,readIORef)
import           MonadLib
import           Control.Monad.IO.Class(MonadIO(..))
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.List



import qualified Galua.Mach             as C -- concrete
import qualified Galua.Value            as C hiding (getTableMeta)
import qualified Galua.Reference        as C
import qualified Galua.Table            as C (tableToList,getTableMeta)
import           Galua.LuaString(toByteString)
import qualified Galua.Micro.Type.Value as A -- abstract
import qualified Galua.Micro.AST        as A
import           Language.Lua.Bytecode(UpIx(..))
import           Language.Lua.Bytecode.FunId(noFun)


importClosure ::
  IORef C.TypeMetatables {- ^ Type metatables      -} ->
  C.Reference C.Table    {- ^ Global environment   -} ->
  C.Reference C.Closure  {- ^ Analysis entry point -} ->
  IO (A.ClosureId, A.TableId, A.GlobalState)
importClosure metas envTable c =
  do let s0 = RW { nextName         = 0
                 , importedUpVals   = []
                 , importedTables   = Map.empty
                 , importedClosures = Map.empty
                 , globs            = A.bottom
                 }
     ((cid,gid), rw) <- runStateT s0 $ unM $
                        do importMetas metas
                           cid <- importFunRef c
                           gid <- importTableRef envTable
                           return (cid,gid)
     return (cid, gid, globs rw)


newtype M a = M { unM :: StateT RW IO a }

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = M (return a)
  (<*>)  = ap

instance Monad M where
  M m >>= f = M (m >>= unM . f)

instance MonadIO M where
  liftIO i = M (inBase i)


data RW = RW
  { nextName          :: !Int
    -- ^ Used to generate external names

    -- References that we've already done
  , importedUpVals    :: ![(IORef C.Value, A.RefId)]
  , importedTables    :: !(Map (C.Reference C.Table)   A.TableId)
  , importedClosures  :: !(Map (C.Reference C.Closure) A.ClosureId)

  
  , globs             :: !A.GlobalState
    -- ^ This is the global state the we build up as we import stuff

  }

newName :: M Int
newName = M $ sets $ \RW { .. } -> (nextName, RW { nextName = 1 + nextName
                                                 , .. })

newExternalRef :: (A.GlobalBlockName -> Int -> a) -> M a
newExternalRef ty = A.externalId ty <$> newName

-- XXX: Maybe we should use the location of the reference somehow...
-- This is not precise enough, because in the analysis the PC is in the
-- micro code, which goes at finer granurality than the op-code PCs.
importNewRef :: (A.GlobalBlockName -> Int -> a) -> C.Reference b -> M a
importNewRef ty _r = newExternalRef ty

importMetas :: IORef C.TypeMetatables -> M ()
importMetas ref =
  do mp <- liftIO (readIORef ref)

     let lkp t = case Map.lookup t mp of
                   Nothing -> return (A.basic A.Nil)
                   Just r  -> do tid <- importTableRef r
                                 return (A.newTable tid)


         setRef tab (t,t') =
           do a <- lkp t
              return (A.letFun t' a tab)

     basicMetas <-
       foldM setRef A.bottom
         [ (C.NilType, A.Nil)
         , (C.NumberType, A.Number)
         , (C.UserDataType, A.UserData)
         , (C.LightUserDataType, A.LightUserData)
         , (C.ThreadType, A.Thread)
         ]

     str <- lkp C.StringType
     fun <- lkp C.FunctionType
     boolean <- lkp C.BoolType

     M $ sets_ $ \RW { .. } ->
                  RW { globs = globs { A.basicMetas = basicMetas
                                     , A.funMeta    = fun
                                     , A.booleanMeta = boolean
                                     , A.stringMeta = str
                                     }, .. }



importUpVal :: IORef C.Value -> M A.RefId
importUpVal r =
  do RW { importedUpVals } <- M get
     case lookup r importedUpVals of
       Just i  -> return i
       Nothing ->
         do i <- newExternalRef A.RefId
            M $ sets_ $ \RW { .. } ->
                         RW { importedUpVals = (r,i) : importedUpVals, .. }
            v <- importValue =<< liftIO (readIORef r)
            M $ sets_ $
                \RW { globs = A.GlobalState { .. }, .. } ->
                 RW { globs = A.GlobalState { heap = Map.insert i v heap, .. },
                                                                          .. }
            return i


importTableRef :: C.Reference C.Table -> M A.TableId
importTableRef r =
  do RW { importedTables } <- M get
     case Map.lookup r importedTables of
       Just i  -> return i
       Nothing ->
        do i <- importNewRef A.TableId r
           M $ sets_ $ \RW { .. } ->
                        RW { importedTables = Map.insert r i importedTables,.. }
           t <- importTable =<< C.readRef r
           M $ sets_ $
             \RW { globs = A.GlobalState { .. }, .. } ->
              RW { globs = A.GlobalState { tables = Map.insert i t tables, .. }
                 , .. }
           return i


importFunRef :: C.Reference C.Closure -> M A.ClosureId
importFunRef r =
  do RW { importedClosures } <- M get
     case Map.lookup r importedClosures of
       Just i  -> return i
       Nothing ->
        do i <- importNewRef A.ClosureId r
           M $ sets_ $ \RW { .. } ->
                        RW { importedClosures = Map.insert r i
                                                   importedClosures ,.. }
           f <- importFunction =<< C.readRef r
           M $ sets_ $
             \RW { globs = A.GlobalState { .. }, .. } ->
              RW { globs = A.GlobalState
                            { functions = Map.insert i f functions, .. }, .. }
           return i



importTable :: C.Table -> M A.TableV
importTable t =
  do ents <- mapM importEntry =<< liftIO (C.tableToList t)
     meta <- importValue      =<< liftIO (C.getTableMeta t)
     let initTab = A.bottom { A.tableFields = A.letFun A.Metatable meta
                                                                    A.bottom }
     return $ foldr addEntry initTab ents
  where
  importEntry (x,y) = (,) <$> importValue x <*> importValue y
  addEntry (k,v) t =
    case A.valueString k of
      A.OneValue s
        | k { A.valueString = A.bottom } == A.bottom ->
          t { A.tableFields = A.letFun (A.Field s) v (A.tableFields t) }

      _ -> t { A.tableKeys   = k A.\/ A.tableKeys t
             , A.tableValues = v A.\/ A.tableValues t
             }


importFunction :: C.Closure -> M A.FunV
importFunction C.MkClosure { .. } =
  do rs <- mapM importUpVal (Vector.toList cloUpvalues)
     let nm = case cloFun of
                C.LuaFunction fid _ -> A.OneValue (A.LuaFunImpl fid)
                C.CFunction ptr     -> A.OneValue (A.CFunImpl (C.cfunAddr ptr))
     return A.FunV { functionUpVals = Map.fromList (zipWith mkRef [ 0 .. ] rs)
                   , functionFID    = nm
                   }
  where
  mkRef i r = (UpIx i, A.OneValue r)



importValue :: C.Value -> M A.Value
importValue val =
  case val of
    C.Bool b          -> return (A.exactBool b)
    C.Number _        -> return (A.basic A.Number)
    C.String s        -> return (A.exactString (toByteString s))
    C.Nil             -> return (A.basic A.Nil)
    C.Table r         -> A.newTable <$> importTableRef r
    C.Closure r       -> A.newFun   <$> importFunRef r
    C.UserData _      -> return (A.basic A.UserData)
    C.LightUserData _ -> return (A.basic A.UserData)
    C.Thread _        -> return (A.basic A.Thread)




