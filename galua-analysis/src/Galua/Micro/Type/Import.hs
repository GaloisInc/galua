-- | Import a concrete value as an abstract value
module Galua.Micro.Type.Import where

import qualified Galua.Value     as GV
import qualified Galua.Reference as GV
import           Galua.Micro.Type.Value


{-
importValue val glob =
  case val of
    GV.Bool _           -> simply Bool
    GV.Number _         -> simply Number
    GV.Nil              -> simply Nil
    GV.UserData _       -> simply UserData
    GV.LightUserData _  -> simply UserData
    GV.Thread _         -> simply Thread
    GV.String s         -> return (StringValue s, glob)
    GV.Table r          -> do t <- readRef r

    GV.Closure r        -> do f <- readRef r
  where
  simply x = return (BasicValue v, glob)
-}
