{-# Language NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
-- | Code for modules loaded in the debugger.
module Galua.Debugger.Source where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IORef(IORef,readIORef,modifyIORef',atomicModifyIORef')
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(decodeUtf8)
import           Data.List(unfoldr,minimumBy)
import           Data.Maybe(catMaybes,maybeToList,fromMaybe)
import           Data.Ord(comparing)

import Galua.Code
import Galua.Debugger.PrettySource (lexChunk,Line,NameId,LocatedExprName)
import Galua.Debugger.Options

-- | All loaded top-level functions.
data Chunks = Chunks
  { topLevelChunks :: Map Int ChunkInfo
    -- ^ The key is the chunk id.  The top-level id for chunk @k@ is @[k]@.

  , allFunNames    :: Map FunId FunVisName
    -- ^ User visiable names for known functions.
  }

-- | All information needed to show a nice user readable function name.
data FunVisName = FunVisName
                    { funVisName        :: !(Maybe Text)
                    , funVisLineStart   :: !Int
                    , funVisLineEnd     :: !Int
                    , funVisFile        :: !(Maybe Text)
                    }


-- | A chunk is akin a module.
data ChunkInfo = ChunkInfo
  { chunkSource   :: Source
  , chunkFunction :: Function
  , chunkLineInfo :: Map Int [(FunId,[Int])]
    -- ^ Map line numbers to the (function & pc) pair that occupies it.
    -- Note that there could be more than one function occupying the same line.
  }



addTopLevel ::
  Maybe String -> ByteString -> Int -> Function -> Chunks -> Chunks
addTopLevel mbName bytes cid fun Chunks { .. } =
  Chunks { topLevelChunks = Map.insert cid newChunk topLevelChunks
         , allFunNames = foldr (uncurry Map.insert)
                               (Map.insert (rootFun cid) chunkName allFunNames)
                             $ concat
                             $ unfoldr nextFun [(rootFun cid,fun)]
         }
  where
  bytes'
    | B.isPrefixOf "\ESCLua" bytes = B.empty
    | otherwise                    = bytes

  newChunk =
    ChunkInfo { chunkSource   = lexSourceFile cid mbName bytes'
              , chunkFunction = fun
              , chunkLineInfo =
                  fmap (\xs -> [ (subFun path cid,n) | (path,n) <- xs ])
                                       $ deepLineNumberMap fun
              }

  chunkName = FunVisName { funVisName      = Just "(top level)"
                         , funVisLineStart = 1
                         , funVisLineEnd   = length (B.lines bytes)
                         , funVisFile      = Text.pack <$> mbName
                         }


  nextFun todo =
    case todo of
      []             -> Nothing
      ((fid,f) : fs) -> Just (out,new ++ fs)
        where
        protos    = zip [ 0 .. ] (Vector.toList (funcNested f))
        subNames  = Map.fromList (inferSubFunctionNames f)
        out       = [ (subFun fid i, funVisName (Map.lookup i subNames) sf)
                                         | (i,sf) <- protos ]
        new       = [ (subFun fid i, sf) | (i,sf) <- protos ]


  funVisName mb f = FunVisName
    { funVisName      = decodeUtf8 <$> mb
    , funVisLineStart = funcLineDefined f
    , funVisLineEnd   = funcLastLineDefined f
    , funVisFile      = fmap Text.pack mbName
    }




-- | Source code for a chunk.
data Source = Source { srcName  :: Maybe String
                     , srcLines :: Vector Line
                     , srcNames :: Map NameId LocatedExprName
                     }


-- | Syntax high-lighting for a source file.
lexSourceFile :: Int -> Maybe String -> ByteString -> Source
lexSourceFile chunkId srcName bytes = Source { srcName, srcLines, srcNames }
  where (srcLines,srcNames) = lexChunk chunkId (fromMaybe "" srcName) bytes

-- | Keep track of the source code for loaded modules.
addSourceFile :: IORef CommandLineBreakPoints ->
                 IORef (Map (Int,FunId) (Maybe break_condition)) ->
                 IORef Chunks ->
                 Maybe String -> ByteString -> Int -> Function -> IO ()
addSourceFile brks breakRef sources mbName bytes cid fun =
  do modifyIORef' sources (addTopLevel mbName bytes cid fun)
     todo <- atomicModifyIORef' brks $ \brkPoints ->
              let del _ _    = Nothing
                  (mb1,brk1) = Map.updateLookupWithKey del "" brkPoints
                  (mb2,brk2) =
                    case mbName of
                      Nothing -> (Nothing, brk1)
                      Just s  -> Map.updateLookupWithKey del s brk1
              in (brk2, concat (maybeToList mb1 ++ maybeToList mb2 ))
     Chunks { topLevelChunks } <- readIORef sources
     case Map.lookup cid topLevelChunks of
       Nothing -> return () -- should not happen
       Just c  ->
         do let lineInfo = chunkLineInfo c
                newBrk   = catMaybes [ findClosest lineInfo ln | ln <- todo ]
            modifyIORef' breakRef $ \old -> foldr addBrk old newBrk

  where
  addBrk b mp = Map.insertWith (\_ y -> y) b Nothing mp

  -- Line 0 is special cased to stop on the first instruction of a chunk.
  findClosest _ 0 = Just (0, rootFun cid)

  findClosest info ln =
    chooseExactLoc $
    case Map.splitLookup ln info of
      (_,Just b,_) -> b
      (_,_,bigger)  | Just (a,_) <- Map.minView bigger  -> a
      (smaller,_,_) | Just (a,_) <- Map.maxView smaller -> a
      _ -> []


  choosePC :: (FunId,[Int]) -> Maybe (Int,FunId)
  choosePC (fid,pcs)
    | null pcs  = Nothing
    | otherwise = Just (minimum pcs, fid)

  chooseExactLoc :: [ (FunId,[Int]) ] -> Maybe (Int,FunId)
  chooseExactLoc funs =
    case funs of
      []   -> Nothing
      [x]  -> choosePC x
      many -> choosePC (minimumBy (comparing (funNestDepth . fst)) many)

