{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Galua.Reference
  ( Alloc, runAlloc
  , AllocRef, exposeAllocRef, runAllocWith
  , NameM(..)
  , referenceId
  , prettyRef
  , addRefFinalizer
  , referenceLoc
  , newReferenceId

  , newRef
  ) where

import           Control.Monad.IO.Class
import           Data.Functor (void)
import           Data.IORef
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import           Numeric(showHex)
import           System.Mem.Weak

import {-# SOURCE #-} Galua.Mach (Thread)
import {-# SOURCE #-} Galua.Value
import qualified Galua.Util.Table as Table
import           Galua.Util.Weak (MakeWeak(makeWeak))
import           Galua.Util.Loc

type Table = Table.Table Value

type WeakMap a = IntMap (Weak a)

data AllocState = AllocState
  { nextReferenceId :: !Int
  , threadRefs      :: !(WeakMap Thread)
  , userDataRefs    :: !(WeakMap UserData)
  , closureRefs     :: !(WeakMap Closure)
  , tableRefs       :: !(WeakMap Table)

  , refLocs         :: !(IntMap RefLoc)
  }

class (MakeWeak a, ReferenceType a) => AllocType a where
  referenceTypeLoc :: Functor f => MonoLoc f (WeakMap a) AllocState

instance AllocType UserData where
  referenceTypeLoc f st =
    st `seq` (\x -> st { userDataRefs = x }) <$> f (userDataRefs st)
  {-# INLINE referenceTypeLoc #-}

instance AllocType Closure where
  referenceTypeLoc f st =
    st `seq` (\x -> st { closureRefs = x }) <$> f (closureRefs st)
  {-# INLINE referenceTypeLoc #-}

instance AllocType Table where
  referenceTypeLoc f st =
    st `seq` (\x -> st { tableRefs = x }) <$> f (tableRefs st)
  {-# INLINE referenceTypeLoc #-}

instance AllocType Thread where
  referenceTypeLoc f st =
    st `seq` (\x -> st { threadRefs = x }) <$> f (threadRefs st)
  {-# INLINE referenceTypeLoc #-}

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


referenceTypeLocOf ::
  (Functor f, AllocType a) => proxy a -> MonoLoc f (WeakMap a) AllocState
referenceTypeLocOf _ = referenceTypeLoc
{-# INLINE referenceTypeLocOf #-}

class MonadIO m => NameM m where
  newRefWithId :: AllocType a => Int -> RefLoc -> a -> m (Reference a)
  newRefId  :: m Int
  lookupRef :: AllocType a => Int -> m (Maybe (Reference a))

instance MonadIO Alloc where
  liftIO m = Alloc (\_ -> m)

instance NameM Alloc where
  newRefWithId i r x = Alloc (newReferenceIOWithId i r x)
  newRefId           = newReferenceId
  lookupRef x = Alloc (lookupReferenceIO x)
  {-# INLINE newRefWithId #-}
  {-# INLINE lookupRef #-}

newRef :: (NameM m, AllocType a) => RefLoc -> a -> m (Reference a)
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
  AllocType a => Int -> RefLoc -> a -> IORef AllocState -> IO (Reference a)
newReferenceIOWithId refId refLoc x stRef =
  do let r = constructReference refId refLoc x

     wx <- makeWeak x (referenceFinalizer stRef r)

     atomicModifyIORef' stRef $ \st ->
        ( setLoc (referenceTypeLoc . intMapAt refId) (Just wx)
        $ setLoc (refLocsLoc       . intMapAt refId) (Just refLoc)
          st, ())

     return $! r
{-# INLINE newReferenceIOWithId #-}

{-
newReferenceIO ::
  ReferenceType a => RefLoc -> a -> IORef AllocState -> IO (Reference a)
newReferenceIO refLoc x stRef =
  do refId <- runAllocWith stRef newReferenceId
     newReferenceIOWithId refId refLoc x stRef
{-# INLINE newReferenceIO #-}
-}

referenceFinalizer ::
  AllocType a => IORef AllocState -> Reference a -> IO ()
referenceFinalizer stRef r =
  atomicModifyIORef' stRef $ \st ->
    ( setLoc (referenceTypeLocOf r . intMapAt (referenceId r)) Nothing
    $ setLoc (refLocsLoc           . intMapAt (referenceId r)) Nothing
      st, ())

lookupReferenceIO ::
  AllocType a => Int -> AllocRef -> IO (Maybe (Reference a))
lookupReferenceIO refId stRef =
  do st <- readIORef stRef
     case getLoc (referenceTypeLoc . intMapAt refId) st of
       Nothing -> return Nothing
       Just wref -> do mbRef <- deRefWeak wref
                       let mbLoc = getLoc (refLocsLoc . intMapAt refId) st
                       return $! constructReference refId <$> mbLoc <*> mbRef

--------------------------------------------------------------------------------


prettyRef :: ReferenceType a => Reference a -> String
prettyRef r = "0x" ++ showHex (referenceId r) ""

addRefFinalizer :: AllocType a => Reference a -> IO () -> IO ()
addRefFinalizer ref finalizer = void (makeWeak (referenceVal ref) finalizer)
