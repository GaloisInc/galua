{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Galua.Stack
  ( Stack
  , empty
  , push
  , pop
  , at
  ) where

import Galua.Loc(MonoLoc)
import Data.Foldable

data Stack a = Stack !Int ![a] -- Size, values
               deriving (Show, Functor, Traversable)

instance Foldable Stack where
  length (Stack n _) = n
  foldr f z (Stack _ xs) = foldr f z xs
  toList (Stack _ xs) = xs

empty :: Stack a
empty = Stack 0 []

push :: a -> Stack a -> Stack a
push a (Stack n xs) = Stack (n+1) (a : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack n (x:xs)) = Just (x, Stack (n-1) xs)
pop _                = Nothing


-- | 0 is the top-most stack index.
at :: Applicative f => Int -> MonoLoc f a (Stack a)
at n0 f (Stack sz as0) = Stack sz <$> listAt n0 as0
  where
  listAt n xs =
    case xs of
      []     -> pure xs
      a : as -> case compare n 0 of
                  LT -> pure xs
                  EQ -> (: as) <$> f a
                  GT -> (a :)  <$> listAt (n - 1) as
