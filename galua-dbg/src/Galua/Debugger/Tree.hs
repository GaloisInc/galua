{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Galua.Debugger.Tree
  ( Tree, leaf, branch, delayed
  , exportTree
  , getDelayed
  ) where

import           Data.IORef

import qualified Data.Aeson as JS


data Tree = Branch String [Tree]
          | Leaf String
          | Delayed (IO Tree)

leaf :: String -> IO Tree
leaf t = return (Leaf t)

branch :: String -> [ IO Tree ] -> IO Tree
branch s xs = Branch s `fmap` sequence xs

delayed :: IO Tree -> IO Tree
delayed m =
  do r <- newIORef Nothing
     return $ Delayed $
        do v <- readIORef r
           case v of
             Just t  -> return t
             Nothing -> do t <- m
                           writeIORef r (Just t)
                           return t


--------------------------------------------------------------------------------

-- | Export a tree in JSON format.
exportTree :: Tree -> JS.Value
exportTree = exportSubTree 1

-- | Get the sub-tree pointed by a link.
getDelayed :: Integer -> Tree -> IO (Maybe JS.Value)
getDelayed p t =
  do mb <- followLink p t
     return (exportSubTree p `fmap` mb)

exportSubTree :: Integer -> Tree -> JS.Value
exportSubTree p (Leaf x)      = p `seq` JS.toJSON x
exportSubTree p (Branch s ts) = con s (zipWith exportBranch [ 1 .. ] ts)
  where exportBranch n t = exportSubTree (n * p) t
exportSubTree p (Delayed _) = JS.toJSON p

con :: String -> [ JS.Value ] -> JS.Value
con t vs = JS.toJSON (JS.toJSON t : vs)


followLink :: Integer -> Tree -> IO (Maybe Tree)
followLink p (Branch _ ts)
  | p > 0 && n > 0 = followLink p1 (ts !! fromIntegral c)
  where n      = length ts
        (p1,c) = divMod p (fromIntegral n)

followLink p (Delayed m)
  | p == 1      = Just `fmap` m
  | p > 1       = followLink p =<< m

followLink _ _ = return Nothing



