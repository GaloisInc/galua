{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Galua.Reference
  ( Alloc, runAlloc
  , AllocRef, exposeAllocRef, runAllocWith
  , NameM(..)
  , Reference, RefLoc(..), CodeLoc(..), ReferenceType
  , referenceId
  , referenceLoc
  , swapRef
  , modifyRef
  , prettyRef
  , addRefFinalizer
  , getRefLoc
  , newReferenceId

  , newRef

  , derefTable
  , derefThread
  , derefUserData
  , derefClosure
  ) where

import Control.Monad.IO.Class
import Data.Function(on)
import Data.IORef
import Data.Functor (void)
import Numeric(showHex)
import System.Mem.Weak
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import Language.Lua.Bytecode.FunId(FunId)
import Galua.Util.Loc
import {-# SOURCE #-} Galua.Mach (Thread)
import {-# SOURCE #-} Galua.Value (Closure, Value, UserData)
import qualified Galua.Util.Table as Table
import Galua.FunValue(CFunName)

type WeakMap a = IntMap (Weak (IORef a))
type Table = Table.Table Value

data AllocState = AllocState
  { nextReferenceId :: !Int
  , threadRefs      :: !(WeakMap Thread)
  , userDataRefs    :: !(WeakMap UserData)
  , closureRefs     :: !(WeakMap Closure)
  , tableRefs       :: !(WeakMap Table)

  , refLocs         :: !(IntMap RefLoc)
  }

emptyAllocState :: AllocState
emptyAllocState = AllocState
  { nextReferenceId = 1
  , threadRefs      = IntMap.empty
  , userDataRefs    = IntMap.empty
  , closureRefs     = IntMap.empty
  , tableRefs       = IntMap.empty
  , refLocs         = IntMap.empty
  }

refLocsLoc :: Functor f => MonoLoc f (IntMap RefLoc) AllocState
refLocsLoc f s = s `seq` (\x -> s { refLocs = x }) <$> f (refLocs s)

type AllocRef = IORef AllocState
newtype Alloc a = Alloc { unAlloc :: AllocRef -> IO a }

instance Functor Alloc where
  fmap f (Alloc g) = Alloc (fmap f . g)
  {-# INLINE fmap #-}

instance Applicative Alloc where
  pure x = Alloc (\_ -> pure x)
  Alloc x <*> Alloc y = Alloc (\e -> x e <*> y e)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Alloc where
  Alloc m >>= f = Alloc (\e -> m e >>= \x -> unAlloc (f x) e)
  {-# INLINE (>>=) #-}

class ReferenceType a where
  referenceTypeLoc :: Functor f => MonoLoc f (WeakMap a) AllocState

instance ReferenceType Thread where
  referenceTypeLoc f st =
    st `seq` (\x -> st { threadRefs = x }) <$> f (threadRefs st)
  {-# INLINE referenceTypeLoc #-}

instance ReferenceType UserData where
  referenceTypeLoc f st =
    st `seq` (\x -> st { userDataRefs = x }) <$> f (userDataRefs st)
  {-# INLINE referenceTypeLoc #-}

instance ReferenceType Closure where
  referenceTypeLoc f st =
    st `seq` (\x -> st { closureRefs = x }) <$> f (closureRefs st)
  {-# INLINE referenceTypeLoc #-}

instance ReferenceType Table where
  referenceTypeLoc f st =
    st `seq` (\x -> st { tableRefs = x }) <$> f (tableRefs st)
  {-# INLINE referenceTypeLoc #-}

referenceTypeLocOf ::
  (Functor f, ReferenceType a) => proxy a -> MonoLoc f (WeakMap a) AllocState
referenceTypeLocOf _ = referenceTypeLoc
{-# INLINE referenceTypeLocOf #-}

class MonadIO m => NameM m where
  newRefWithId :: ReferenceType a => Int -> RefLoc -> a -> m (Reference a)
  newRefId  :: m Int
  lookupRef :: ReferenceType a => Int -> m (Maybe (Reference a))

instance MonadIO Alloc where
  liftIO m = Alloc (\_ -> m)

instance NameM Alloc where
  newRefWithId i r x = Alloc (newReferenceIOWithId i r x)
  newRefId           = newReferenceId
  lookupRef x = Alloc (lookupReferenceIO x)
  {-# INLINE newRefWithId #-}
  {-# INLINE lookupRef #-}

newRef :: (NameM m, ReferenceType a) => RefLoc -> a -> m (Reference a)
newRef loc x =
  do i <- newRefId
     newRefWithId i loc x

runAllocWith :: AllocRef -> Alloc a -> IO a
runAllocWith aref (Alloc m) = m aref

exposeAllocRef :: Alloc AllocRef
exposeAllocRef = Alloc return

runAlloc :: Alloc a -> IO a
runAlloc m = do ref <- newIORef emptyAllocState
                runAllocWith ref m

newReferenceId :: Alloc Int
newReferenceId = Alloc $ \ref ->
  atomicModifyIORef' ref $
    \st@AllocState{ nextReferenceId = i } ->
    (st { nextReferenceId = i+1 }, i)

newReferenceIOWithId ::
  ReferenceType a => Int -> RefLoc -> a -> IORef AllocState -> IO (Reference a)
newReferenceIOWithId refId refLoc x stRef =
  do ref   <- newIORef x

     let r = Reference
               { referenceId  = refId
               , referenceRef = ref
               , referenceLoc = refLoc
               }

     wref <- mkWeakIORef ref (referenceFinalizer stRef r)

     atomicModifyIORef' stRef $ \st ->
        ( setLoc (referenceTypeLoc . intMapAt refId) (Just wref)
        $ setLoc (refLocsLoc       . intMapAt refId) (Just refLoc)
          st, ())

     return $! r
{-# INLINE newReferenceIOWithId #-}

newReferenceIO ::
  ReferenceType a => RefLoc -> a -> IORef AllocState -> IO (Reference a)
newReferenceIO refLoc x stRef =
  do refId <- runAllocWith stRef newReferenceId
     newReferenceIOWithId refId refLoc x stRef
{-# INLINE newReferenceIO #-}

referenceFinalizer ::
  ReferenceType a => IORef AllocState -> Reference a -> IO ()
referenceFinalizer stRef r =
  atomicModifyIORef' stRef $ \st ->
    ( setLoc (referenceTypeLocOf r . intMapAt (referenceId r)) Nothing
    $ setLoc (refLocsLoc           . intMapAt (referenceId r)) Nothing
      st, ())

lookupReferenceIO ::
  ReferenceType a => Int -> AllocRef -> IO (Maybe (Reference a))
lookupReferenceIO refId stRef =
  do st <- readIORef stRef
     case getLoc (referenceTypeLoc . intMapAt refId) st of
       Nothing -> return Nothing
       Just wref -> do mbRef <- deRefWeak wref
                       let mbLoc = getLoc (refLocsLoc . intMapAt refId) st
                       return $! Reference refId <$> mbRef <*> mbLoc

--------------------------------------------------------------------------------

data Reference a = Reference
  { referenceId  :: {-# UNPACK #-}!Int
  , referenceRef :: {-# UNPACK #-}!(IORef a)
  , referenceLoc :: !(RefLoc)
  }


{-
data family Reference a

data instance Reference Table =
  TableRef
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !RefLoc
    {-# UNPACK #-} !Table

data instance Reference UserData =
  UserDataRef
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !RefLoc
    {-# UNPACK #-} !(IORef UserData)

data instance Reference Thread =
  ThreadRef
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !RefLoc
    {-# UNPACK #-} !(IORef Thread)

data instance Reference Closure =
  ClosureRef
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !RefLoc
    {-# UNPACK #-} !(IORef Closure)
-}

-- | The location in the source code that allocated this reference.
data RefLoc  = RefLoc { refLocCaller :: !CodeLoc, refLocSite :: !CodeLoc }
               deriving (Eq,Ord)

data CodeLoc = MachSetup
             | InC !CFunName
             | InLua !FunId !Int  -- ^ Function, program counter
               deriving (Eq,Ord)

instance Show (Reference a) where
  show = prettyRef

instance Eq (Reference a) where
  (==) = (==) `on` referenceId
  (/=) = (/=) `on` referenceId

instance Ord (Reference a) where
  compare = compare `on` referenceId
  (<=) = (<=) `on` referenceId
  (<)  = (<)  `on` referenceId
  (>)  = (>)  `on` referenceId
  (>=) = (>=) `on` referenceId

prettyRef :: Reference a -> String
prettyRef (Reference x _ _) = "0x" ++ showHex x ""

swapRef :: MonadIO m => Reference a -> a -> m a
swapRef ref new =
  liftIO (atomicModifyIORef (referenceRef ref) (\old -> (new,old)))

modifyRef :: MonadIO m => Reference a -> (a -> a) -> m ()
modifyRef ref f = liftIO (modifyIORef' (referenceRef ref) f)

addRefFinalizer :: Reference a -> IO () -> IO ()
addRefFinalizer ref finalizer = void (mkWeakIORef (referenceRef ref) finalizer)

getRefLoc :: Reference a -> RefLoc
getRefLoc = referenceLoc

readRef :: MonadIO m => Reference a -> m a
readRef = liftIO . readIORef . referenceRef

derefTable :: MonadIO m => Reference Table -> m Table
derefTable = readRef

derefThread :: MonadIO m => Reference Thread -> m Thread
derefThread = readRef

derefUserData :: MonadIO m => Reference UserData -> m UserData
derefUserData = readRef

derefClosure :: MonadIO m => Reference Closure -> m Closure
derefClosure = readRef
