module Galua.Util.SmallVec where

import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import Control.Monad(replicateM_)

data SmallVec a = Vec0
                | Vec1 a
                | Vec2 a a
                | VecMany {-# UNPACK #-} !(Vector a)  -- ^ at least 3

-- | Is this a vector with exactly 2 elements.
isVec2 :: SmallVec a -> Maybe (a,a)
isVec2 vs =
  case vs of
    Vec2 a b -> Just (a,b)
    _        -> Nothing

-- | Generate a vector of the given lenght, using the IO function
-- to initialize it.
generateM :: Int -> (Int -> IO a) -> IO (SmallVec a)
generateM n f =
  case n of
    0 -> return Vec0
    1 -> Vec1 <$> f 0
    2 -> Vec2 <$> f 0 <*> f 1
    n -> VecMany <$> Vector.generateM n f

(++) :: SmallVec a -> SmallVec a -> SmallVec a
xs ++ ys =
  case xs of
    Vec0     -> ys
    Vec1 x   -> cons x ys
    Vec2 x y -> case ys of
                  Vec0        -> xs
                  Vec1 a      -> VecMany (Vector.fromListN 3 [x,y,a])
                  Vec2 a b    -> VecMany (Vector.fromListN 4 [x,y,a,b])
                  VecMany vs  -> VecMany (Vector.cons x (Vector.cons y vs))

    VecMany vs ->
      case ys of
        Vec0        -> xs
        Vec1 a      -> VecMany (Vector.snoc vs a)
        Vec2 a b    -> VecMany (Vector.snoc (Vector.snoc vs a) b)
        VecMany ws  -> VecMany (vs Vector.++ ws)

iForM_ :: SmallVec a -> (Int -> a -> IO ()) -> IO ()
iForM_ vs f =
  case vs of
    Vec0        -> return ()
    Vec1 x      -> f 0 x
    Vec2 x y    -> f 0 x >> f 1 y
    VecMany xs  -> Vector.forM_ (Vector.indexed xs) (uncurry f)

forM_ :: SmallVec a -> (a -> IO ()) -> IO ()
forM_ vs f =
  case vs of
    Vec0        -> return ()
    Vec1 x      -> f x
    Vec2 x y    -> f x >> f y
    VecMany xs  -> Vector.forM_ xs f

padForM_ :: SmallVec a -> Int -> a -> (a -> IO ()) -> IO ()
padForM_ vs n d f =
  case vs of
    Vec0        -> replicateM_ n (f d)
    Vec1 x      -> do if n == 0 then return () else f x
                      replicateM_ (n - 1) (f d)
    Vec2 x y    -> case n of
                     0 -> return ()
                     1 -> f x
                     _ -> f x >> f y >> replicateM_ (n - 2) (f d)

    VecMany xs  -> do Vector.forM_ (Vector.take n xs) f
                      replicateM_ (n - Vector.length xs) (f d)


ipadForM_ :: SmallVec a -> Int -> a -> (Int -> a -> IO ()) -> IO ()
ipadForM_ vs n d f =
  case vs of
    Vec0        -> useDefault 0
    Vec1 x      -> do if n == 0 then return () else f 0 x
                      useDefault 1
    Vec2 x y    -> case n of
                     0 -> return ()
                     1 -> f 0 x
                     _ -> f 0 x >> f 1 y >> useDefault 2

    VecMany xs  -> do Vector.forM_ (Vector.indexed (Vector.take n xs))
                                                                    (uncurry f)
                      useDefault (Vector.length xs)


  where
  useDefault i = zipWithM_ f [ i .. n - 1 ] (repeat d)



thaw :: SmallVec a -> IO (IOVector a)
thaw vs =
  case vs of
    Vec0 -> IOVector.new 0
    Vec1 x -> do v <- IOVector.new 1
                 IOVector.unsafeWrite v 0 x
                 return v
    Vec2 x y -> do v <- IOVector.new 2
                   IOVector.unsafeWrite v 0 x
                   IOVector.unsafeWrite v 1 y
                   return v
    VecMany xs -> Vector.thaw xs


freeze :: IOVector a -> IO (SmallVec a)
freeze v = case IOVector.length v of
             0 -> return Vec0
             1 -> Vec1 <$> IOVector.unsafeRead v 0
             2 -> Vec2 <$> IOVector.unsafeRead v 0 <*> IOVector.unsafeRead v 1
             _ -> VecMany <$> Vector.freeze v

unsafeIndex :: SmallVec a -> Int -> a
unsafeIndex vs n =
  case vs of
    Vec0        -> error "Index out of bounds."
    Vec1 x      -> x
    Vec2 x y    -> if n == 0 then x else y
    VecMany xs  -> Vector.unsafeIndex xs n

length :: SmallVec a -> Int
length vs =
  case vs of
    Vec0        -> 0
    Vec1 {}     -> 1
    Vec2 {}     -> 2
    VecMany xs  -> Vector.length xs

cons :: a -> SmallVec a -> SmallVec a
cons a vs =
  case vs of
    Vec0        -> Vec1 a
    Vec1 x      -> Vec2 a x
    Vec2 x y    -> VecMany (Vector.fromListN 3 [a,x,y])
    VecMany xs  -> VecMany (Vector.cons a xs)

uncons :: SmallVec a -> Maybe (a,SmallVec a)
uncons vs =
  case vs of
    Vec0       -> Nothing
    Vec1 x     -> Just (x,Vec0)
    Vec2 x y   -> Just (x,Vec1 y)
    VecMany xs -> Just (Vector.unsafeHead xs, fromVector (Vector.tail xs))

maybeHead :: SmallVec a -> a -> a
maybeHead v a =
  case v of
    Vec0        -> a
    Vec1 x      -> x
    Vec2 x _    -> x
    VecMany xs  -> Vector.unsafeHead xs

empty :: SmallVec a
empty = Vec0

vec1 :: a -> SmallVec a
vec1 = Vec1

vec2 :: a -> a -> SmallVec a
vec2 = Vec2

vec3 :: a -> a -> a -> SmallVec a
vec3 a b c = VecMany (Vector.fromListN 3 [a,b,c])

fromList :: [a] -> SmallVec a
fromList xs =
  case xs of
    []    -> Vec0
    [x]   -> Vec1 x
    [x,y] -> Vec2 x y
    _     -> VecMany (Vector.fromList xs)

fromVector :: Vector a -> SmallVec a
fromVector vs =
  case Vector.length vs of
    0 -> Vec0
    1 -> Vec1 (Vector.unsafeHead vs)
    2 -> Vec2 (Vector.unsafeHead vs) (Vector.unsafeIndex vs 1)
    _ -> VecMany vs


toList :: SmallVec a -> [a]
toList vs =
  case vs of
    Vec0        -> []
    Vec1 x      -> [x]
    Vec2 x y    -> [x,y]
    VecMany xs  -> Vector.toList xs








