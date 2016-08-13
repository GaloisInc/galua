{-# LANGUAGE OverloadedStrings #-}
module Galua.Debugger.PrettySource (Line, lexChunk, omittedLine) where


import Language.Lua.Annotated.Lexer
          (llexNamedWithWhiteSpace,LexToken(..),SourcePos(..))
import Language.Lua.Annotated.Parser(parseText,chunk)
import qualified Language.Lua.Token as L

import Galua.Names.Find(chunkLocations,ExprName(..),LocatedExprName(..))

import Debug.Trace


import           Data.Function(on)
import           Data.List(groupBy,sortBy)
import qualified Data.ByteString as BS
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(decodeUtf8With)
import           Data.Text.Encoding.Error(lenientDecode)

import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..), (.=))

newtype Line    = Line [Token]
                  deriving Show

data Token      = Token TokenType Text [ExprName]
                  deriving Show

data TokenType  = Keyword | Operator | Symbol | Ident | Literal
                | Comment | WhiteSpace | Error
                  deriving Show

omittedLine :: Line
omittedLine = Line [Token Comment "..." []]

-- XXX: This lexes the file twice.
lexChunk :: String -> BS.ByteString -> Vector Line
lexChunk name bytes = trace (show names) $ 
      Vector.fromList
    $ map (Line . map token)
    $ groupBy ((==) `on` aTokLine)
    $ addNames names
    $ concatMap splitTok
    $ llexNamedWithWhiteSpace name txt
  where
  txt   = decodeUtf8With lenientDecode bytes

  names = sortBy (compare `on` (sourcePosIndex . exprPos))
        $ case parseText chunk txt of
            Left _  -> []
            Right b -> chunkLocations b

data AnnotToken = AnnotToken (LexToken SourcePos) [ExprName]

aTokLine :: AnnotToken -> Int
aTokLine (AnnotToken l _) = sourcePosLine (ltokPos l)

-- XXX: Make it better
addNames :: [LocatedExprName] -> [LexToken SourcePos] -> [AnnotToken]
addNames ns (t:ts) =
  let thisIx       = tokIx t
      (here,later) = span ((<= thisIx) . nameIx) ns
  in AnnotToken t (map exprName here) : addNames later ts
  where
  nameIx = sourcePosIndex . exprPos
  tokIx  = sourcePosIndex . ltokPos
addNames _ []  = []


instance ToJSON Line where
  toJSON (Line xs) = toJSON xs

instance ToJSON Token where
  toJSON (Token x y t) = JS.object [ "token" .= x, "lexeme" .= y
                                   , "names" .= show t ] -- XXX

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

-- String and comment tokens may span multiple lines, so we split them
-- into multiple tokens---one per line.
splitTok :: LexToken SourcePos -> [LexToken SourcePos]
splitTok tok = map mk (zip [ 0 .. ] (split (ltokLexeme tok)))
  where
  mk (n,s)  = tok { ltokLexeme = s, ltokPos = incBy n (ltokPos tok) }
  incBy n p = p { sourcePosLine = n + sourcePosLine p }

  split cs = case Text.break (== '\n') cs of
               (as,bs)
                  | Text.null bs   -> [ as ]
                  | otherwise      -> as : split (Text.tail bs)

token :: AnnotToken -> Token
token (AnnotToken tok ns) = Token (tokenType tok) (ltokLexeme tok) ns

tokenType :: LexToken a -> TokenType
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

    L.TokNum         -> Literal
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

