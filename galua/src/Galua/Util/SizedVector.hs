{-# LANGUAGE RecordWildCards #-}
module Galua.Util.SizedVector
  ( SizedVector
  , new
  , newN
  , push
  , pop
  , popN
  , size
  , shrink
  , set
  , setMaybe
  , get
  , getLastN
  , unsafeGet
  , getMaybe
  , rotateSubset
  , fromList
  , toList
  , shallowCopy
  ) where

import Data.IORef
import qualified Data.Vector.Mutable as IOVector
import           Data.Vector ( Vector )
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector)
import Data.Foldable (for_)
import Control.Exception
import Control.Monad

newtype SizedVector a = SizedVector (IORef (SizedVector' a))

data SizedVector' a = SizedVector'
  { svCount :: {-# UNPACK #-} !Int
  , svArray :: {-# UNPACK #-} !(IOVector a)
  }

data SizedVectorException = SizedVectorException String
  deriving (Read, Show)

instance Exception SizedVectorException

failure :: String -> IO a
failure str = throwIO (SizedVectorException str)

initialAllocation :: Int
initialAllocation = 4

-- | Allocate a new, empty, resizable vector with a default initial allocation.
new :: IO (SizedVector a)
new = newN initialAllocation

-- | Allocate a new, empty, resizable vector with a given initial allocation
-- size.
newN :: Int {- ^ initial allocation -} -> IO (SizedVector a)
newN n =
  do let svCount = 0
     svArray <- IOVector.unsafeNew (max initialAllocation n)
     SizedVector <$> newIORef SizedVector'{..}

-- | The "top" of the stack is the end
push :: SizedVector a -> a -> IO ()
push (SizedVector ref) x =
  do SizedVector'{..} <- readIORef ref

     v <- if svCount < IOVector.length svArray
            then return svArray
            else do let l = IOVector.length svArray
                    unless (l < maxBound`div`2) (failure "push: vector too big")
                    let l' = max initialAllocation
                              -- avoid doubling 0 due to fromList
                           $ 2*IOVector.length svArray
                    IOVector.unsafeGrow svArray l'

     IOVector.write v svCount x
     writeIORef ref $! SizedVector' (svCount+1) v

-- | The "top" is the end
pop :: SizedVector a -> IO a
pop (SizedVector ref) =
  do SizedVector' { .. } <- readIORef ref
     when (svCount < 1) (failure "pop: empty stack")
     let end = svCount - 1
     v <- IOVector.unsafeRead svArray end
     IOVector.unsafeWrite svArray end (error "uninitialized")
     writeIORef ref $! SizedVector' { svCount = svCount - 1, .. }
     return v

shrink :: SizedVector a -> Int -> IO ()
shrink (SizedVector ref) n =
  do SizedVector'{..} <- readIORef ref
     unless (0 <= n && n <= svCount) (failure "shrink: bad argument")

     let svCount' = svCount - n

     for_ [svCount' .. svCount-1] $ \i ->
       IOVector.unsafeWrite svArray i (error "uninitialized")

     writeIORef ref $! SizedVector' svCount' svArray

size :: SizedVector a -> IO Int
size (SizedVector ref) = svCount <$> readIORef ref

get :: SizedVector a -> Int -> IO a
get sv i = maybe (failure "Get: bad index") return =<< getMaybe sv i

-- | Get the elements from the given index to then end (aka "top") of the stack.
getLastN :: SizedVector a -> Int -> IO (Vector a)
getLastN (SizedVector ref) len0 =
  do SizedVector' { .. } <- readIORef ref
     let len  = max 0 (min len0 svCount)
         from = svCount - len
     Vector.freeze (IOVector.unsafeSlice from len svArray)

-- | Pop the last elements from the stack.
popN :: SizedVector a -> Int -> IO (Vector a)
popN (SizedVector ref) len0 =
  do SizedVector' { .. } <- readIORef ref
     let len  = max 0 (min len0 svCount)
         from = svCount - len
     v <- Vector.freeze (IOVector.unsafeSlice from len svArray)
     for_ [ from .. svCount - 1] $ \i ->
       IOVector.unsafeWrite svArray i (error "uninitialized")
     writeIORef ref $! SizedVector' { svCount = svCount - len, .. }
     return v




unsafeGet :: SizedVector a -> Int -> IO a
unsafeGet (SizedVector ref) i =
  do SizedVector'{..} <- readIORef ref
     IOVector.unsafeRead svArray i

getMaybe :: SizedVector a -> Int -> IO (Maybe a)
getMaybe (SizedVector ref) i =
  do SizedVector'{..} <- readIORef ref
     if 0 <= i && i < svCount
       then Just <$> IOVector.unsafeRead svArray i
       else return Nothing

set :: SizedVector a -> Int -> a -> IO ()
set (SizedVector ref) i x =
  do SizedVector'{..} <- readIORef ref
     unless (0 <= i && i < svCount) (failure "set: bad index")
     IOVector.unsafeWrite svArray i x

setMaybe :: SizedVector a -> Int -> a -> IO ()
setMaybe (SizedVector ref) i x =
  do SizedVector'{..} <- readIORef ref
     when (0 <= i && i < svCount) (IOVector.unsafeWrite svArray i x)



-- | Rotate a subset of a vector by a given offset. Positive rotations advance
-- elements to the right. Negative rotations advance elements to the left.
rotateSubset ::
  SizedVector a ->
  Int {- ^ initial index -} ->
  Int {- ^ count         -} ->
  Int {- ^ offset        -} ->
  IO ()
rotateSubset v start count offset
  | offset < 0 = leftRotateSubset v start count (-offset)
  | otherwise  = leftRotateSubset v start count (count-offset)

leftRotateSubset ::
  SizedVector a ->
  Int {- ^ initial index -} ->
  Int {- ^ count         -} ->
  Int {- ^ offset        -} ->
  IO ()
leftRotateSubset (SizedVector ref) start count offset =
  do SizedVector'{..} <- readIORef ref

     unless (0 <= start && 0 <= count && start <= svCount - count)
            (failure "rotateSubset: bad start/count")
     unless (0 <= offset && offset <= count)
            (failure "rotateSubset: bad offset")

     let rev i n = rev' i (i+n-1)

         rev' l r = when (l < r)
                  $ do IOVector.unsafeSwap svArray l r
                       rev' (l+1) (r-1) :: IO ()

     rev start offset
     rev (start+offset) (count - offset)
     rev start count

fromList :: [a] -> IO (SizedVector a)
fromList xs =
  do a <- Vector.thaw (Vector.fromList xs)
     let sv = SizedVector'{ svArray = a, svCount = IOVector.length a }
     SizedVector <$> newIORef sv

toList :: SizedVector a -> IO [a]
toList (SizedVector ref) =
  do SizedVector'{..} <- readIORef ref
     Vector.toList <$> Vector.freeze (IOVector.unsafeTake svCount svArray)

shallowCopy :: SizedVector a -> IO (SizedVector a)
shallowCopy (SizedVector ref) =
  do v  <- readIORef ref
     v' <- IOVector.clone (svArray v)
     SizedVector <$> newIORef v{svArray = v'}
