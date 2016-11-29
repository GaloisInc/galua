{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Galua.Reference
  ( AllocRef
  , AllocType
  , referenceId
  , prettyRef
  , addRefFinalizer
  , referenceLoc
  , newRefId
  , newRefWithId
  , newRef
  , lookupRef
  , newAllocRef
  ) where

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

type AllocRef = IORef AllocState

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


newAllocRef :: IO AllocRef
newAllocRef = newIORef emptyAllocState

refLocsLoc :: Functor f => MonoLoc f (IntMap RefLoc) AllocState
refLocsLoc f s = s `seq` (\x -> s { refLocs = x }) <$> f (refLocs s)



referenceTypeLocOf ::
  (Functor f, AllocType a) => proxy a -> MonoLoc f (WeakMap a) AllocState
referenceTypeLocOf _ = referenceTypeLoc
{-# INLINE referenceTypeLocOf #-}


newRef :: AllocType a => AllocRef -> RefLoc -> a -> IO (Reference a)
newRef aref loc x =
  do i <- newRefId aref
     newRefWithId aref i loc x

newRefId :: AllocRef -> IO Int
newRefId ref = atomicModifyIORef' ref $
                 \st@AllocState{ nextReferenceId = i } ->
                 (st { nextReferenceId = i+1 }, i)


newRefWithId ::
  AllocType a => AllocRef -> Int -> RefLoc -> a -> IO (Reference a)
newRefWithId stRef refId refLoc x =
  do let r = constructReference refId refLoc x

     wx <- makeWeak x (referenceFinalizer stRef r)

     atomicModifyIORef' stRef $ \st ->
        ( setLoc (referenceTypeLoc . intMapAt refId) (Just wx)
        $ setLoc (refLocsLoc       . intMapAt refId) (Just refLoc)
          st, ())

     return $! r


referenceFinalizer :: AllocType a => AllocRef -> Reference a -> IO ()
referenceFinalizer stRef r =
  atomicModifyIORef' stRef $ \st ->
    ( setLoc (referenceTypeLocOf r . intMapAt (referenceId r)) Nothing
    $ setLoc (refLocsLoc           . intMapAt (referenceId r)) Nothing
      st, ())

lookupRef :: AllocType a => AllocRef -> Int -> IO (Maybe (Reference a))
lookupRef stRef refId =
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
