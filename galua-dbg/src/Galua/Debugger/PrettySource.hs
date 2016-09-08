{-# LANGUAGE OverloadedStrings #-}
module Galua.Debugger.PrettySource
  ( Line, lexChunk, omittedLine
  , NameId(..), LocatedExprName
  , lineToJSON
  ) where


import Language.Lua.Annotated.Lexer
          (llexNamedWithWhiteSpace,LexToken(..),SourcePos(..),SourceRange(..)
          , dropWhiteSpace)
import Language.Lua.Annotated.Parser(parseTokens,chunk)
import qualified Language.Lua.Token as L
import Language.Lua.Bytecode.FunId (FunId, funIdList)

import Galua.Names.Find (chunkLocations,LocatedExprName(..))
import Galua.Debugger.View.Utils (exportFID)

import           Data.Function(on)
import           Data.List(groupBy,sortBy)
import qualified Data.ByteString as BS
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(decodeUtf8With)
import           Data.Text.Encoding.Error(lenientDecode)

import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..), (.=))

newtype Line    = Line [Token]
                  deriving Show

data Token      = Token TokenType Text (Maybe NameId) [NameId] (Maybe FunId)
                  deriving Show

data TokenType  = Keyword | Operator | Symbol | Ident | Literal
                | Comment | WhiteSpace | Error
                  deriving Show

omittedLine :: Line
omittedLine = Line [Token Comment "..." Nothing [] Nothing]

lexChunk :: Int -> String -> BS.ByteString -> (Vector Line, Map NameId LocatedExprName)
lexChunk chunkId name bytes =
    ( Vector.fromList
      $ map (Line . map token)
      $ groupBy ((==) `on` aTokLine)
      $ addFunIds functionNames
      $ addNames names
      $ concatMap splitTok
      $ tokens
    , nameMap
    )
  where

  txt    = decodeUtf8With lenientDecode bytes

  tokens = llexNamedWithWhiteSpace name txt

  (flatNames, functionNames) =
    case parseTokens chunk (dropWhiteSpace tokens) of
      Left _err -> ([],[])
      Right b   -> chunkLocations chunkId b

  names = map (sortBy (compare `on` endIx))
        $ groupBy ((==) `on` startIx)
        $ sortBy (compare `on` startIx) flatNames

  nameMap = Map.fromList [ (nameId x, x) | x <- flatNames ]

class Rng t where
  getRange :: t -> SourceRange

instance Rng LexToken where
  getRange = ltokRange

instance Rng LocatedExprName where
  getRange = exprPos

instance Rng AnnotToken where
  getRange (AnnotToken a _ _ _) = getRange a

startIx :: Rng t => t -> Int
startIx = sourcePosIndex . sourceFrom . getRange

endIx :: Rng t => t -> Int
endIx = sourcePosIndex . sourceTo . getRange


data AnnotToken = AnnotToken LexToken (Maybe NameId) [NameId] (Maybe FunId)

aTokLine :: AnnotToken -> Int
aTokLine = sourcePosLine . sourceFrom . getRange


data NameId = NameId !Int !Int
              deriving (Show,Eq,Ord)

nameId :: LocatedExprName -> NameId
nameId e = NameId (startIx e) (endIx e)

setFunId :: FunId -> AnnotToken -> AnnotToken
setFunId funId (AnnotToken a b c _) = AnnotToken a b c (Just funId)

addFunIds :: [FunId] -> [AnnotToken] -> [AnnotToken]
addFunIds [] ys = ys
addFunIds _  [] = [] -- not good, but keep going
addFunIds xxs@(funId:xs) (y:ys)
  | isFunction y = setFunId funId y : addFunIds xs ys
  | otherwise = y : addFunIds xxs ys

isFunction (AnnotToken t _ _ _) = ltokToken t == L.TokFunction

addNames :: [ [LocatedExprName] ] -> [LexToken] -> [ AnnotToken ]
addNames = go []
  where

  annot t mb stack = AnnotToken t (nameId <$> mb) (map nameId stack) Nothing

  -- ensure top of the stack, if any, is active
  go (s : stack) ns (t : ts)
    | endIx s < startIx t = go stack ns (t : ts)

  -- shouldn't happen but here for completenes, and no warnings
  go stack ([] : nss) ts = go stack nss ts

  -- n is not active, use the stack
  go st@(s : stack) nss@((n : _) : _) (t : ts)
    | startIx n > endIx t =
      annot t (Just s) st : go (s : stack) nss ts

  -- no more names, use the stack
  go st@(s : stack) [] (t : ts) =
      annot t (Just s) st : go (s : stack) [] ts

  -- n is not active, no stack
  go [] nss@((n : _) : _) (t : ts)
    | startIx n > endIx t =
       annot t Nothing [] : go [] nss ts

  -- no more names or stack, just copy token.
  go [] [] (t : ts) =
       annot t Nothing [] : go [] [] ts

  -- new active name
  go stack ((n:ns) : nss) (t : ts) =
      let stack' = n : ns ++ stack
      in annot t (Just n) stack' : go stack' nss ts

  go _ _ [] = []




lineToJSON :: (NameId -> Bool) -> Line -> JS.Value
lineToJSON inScope (Line ts) = toJSON $ map (tokenToJSON inScope) ts

tokenToJSON :: (NameId -> Bool) -> Token -> JS.Value
tokenToJSON inScope (Token x y t cs funId) =
  JS.object [ "token"   .= x
            , "lexeme"  .= y
            , "name"    .= (link <$> t)
            , "names"   .= cs
            , "funid"   .= (exportFID <$> funId)
            ]
  where
  link z = JS.object [ "ref" .= z, "active" .= inScope z ]

instance ToJSON NameId where
  toJSON (NameId x y) = toJSON (show x ++ "_" ++ show y)

instance ToJSON TokenType where
  toJSON l = toJSON
           $ case l of
               Keyword    -> "keyword" :: Text
               Operator   -> "operator"
               Symbol     -> "symbol"
               Ident      -> "identifier"
               Literal    -> "literal"
               Comment    -> "comment"
               WhiteSpace -> "white_space"
               Error      -> "error"





--------------------------------------------------------------------------------

-- | String and comment tokens may span multiple lines, so we split them
-- into multiple tokens---one per line.
splitTok :: LexToken -> [LexToken]
splitTok = split
  where
  split tok =
    case Text.break (== '\n') (ltokLexeme tok) of
      (as,bs)
        | Text.null bs -> if Text.null as then [] else [tok]
        | otherwise ->
          let rng  = ltokRange tok
              from = sourceFrom rng
              to   = sourceFrom rng
              len  = Text.length as
              ix   = sourcePosIndex from + len
              to'  = from { sourcePosColumn = sourcePosColumn from + len
                          , sourcePosIndex  = ix
                          }
              from' = from { sourcePosColumn  = 1
                           , sourcePosLine    = sourcePosLine from + 1
                           , sourcePosIndex   = ix + 2
                             -- the char after the \n
                           }

              t1 = tok { ltokLexeme = as
                       , ltokRange  = SourceRange from to'
                       }
              t2 = tok { ltokLexeme = Text.tail bs
                       , ltokRange  = SourceRange from' to
                       }

          in t1 : split t2


token :: AnnotToken -> Token
token (AnnotToken tok ns cs funId) =
  Token (tokenType tok) (ltokLexeme tok) ns cs funId

tokenType :: LexToken -> TokenType
tokenType tok =
  case ltokToken tok of
    L.TokPlus        -> Operator
    L.TokMinus       -> Operator
    L.TokStar        -> Operator
    L.TokSlash       -> Operator
    L.TokPercent     -> Operator
    L.TokExp         -> Operator
    L.TokEqual       -> Operator
    L.TokNotequal    -> Operator
    L.TokLEq         -> Operator
    L.TokGEq         -> Operator
    L.TokLT          -> Operator
    L.TokGT          -> Operator
    L.TokAssign      -> Operator
    L.TokDDot        -> Operator
    L.TokDLT         -> Operator
    L.TokDGT         -> Operator
    L.TokAmpersand   -> Operator
    L.TokPipe        -> Operator
    L.TokDSlash      -> Operator
    L.TokTilde       -> Operator
    L.TokSh          -> Operator

    L.TokColon       -> Operator
    L.TokDot         -> Operator

    L.TokAnd         -> Keyword
    L.TokBreak       -> Keyword
    L.TokDo          -> Keyword
    L.TokElse        -> Keyword
    L.TokElseIf      -> Keyword
    L.TokEnd         -> Keyword
    L.TokFalse       -> Keyword
    L.TokFor         -> Keyword
    L.TokFunction    -> Keyword
    L.TokGoto        -> Keyword
    L.TokIf          -> Keyword
    L.TokIn          -> Keyword
    L.TokLocal       -> Keyword
    L.TokNil         -> Keyword
    L.TokNot         -> Keyword
    L.TokOr          -> Keyword
    L.TokRepeat      -> Keyword
    L.TokReturn      -> Keyword
    L.TokThen        -> Keyword
    L.TokTrue        -> Keyword
    L.TokUntil       -> Keyword
    L.TokWhile       -> Keyword

    L.TokInt         -> Literal
    L.TokFloat       -> Literal
    L.TokSLit        -> Literal

    L.TokIdent       -> Ident

    L.TokLParen      -> Symbol
    L.TokRParen      -> Symbol
    L.TokLBrace      -> Symbol
    L.TokRBrace      -> Symbol
    L.TokLBracket    -> Symbol
    L.TokRBracket    -> Symbol
    L.TokDColon      -> Symbol
    L.TokSemic       -> Symbol
    L.TokComma       -> Symbol
    L.TokEllipsis    -> Symbol

    L.TokComment     -> Comment
    L.TokWhiteSpace  -> WhiteSpace

    L.TokUnexpected    -> Error
    L.TokUntermString  -> Error
    L.TokUntermComment -> Error

