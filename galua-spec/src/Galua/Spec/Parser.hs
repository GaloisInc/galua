{-# Language OverloadedStrings #-}
module Galua.Spec.Parser
  ( module Galua.Spec.Parser
  , Lexeme(..)
  , SourceRange(..)
  , SourcePos(..)
  , Parsed
  ) where

import           Data.Text(Text)
import qualified Data.Text.IO as Text
import           Control.Exception (throwIO)

import Galua.Spec.AST(Spec)
import Galua.Spec.Parser.Grammar(parseSpec,Parsed)
import Galua.Spec.Parser.Monad(runParser,ParseError)
import Galua.Spec.Parser.Lexer
         (lexer,initialInput,Lexeme(..),Token(..),SourceRange(..),SourcePos(..))
import Galua.Spec.Parser.Layout(addLayoutTokens)

-- | Construct a list of lexemes.  Preserves white space.
lexText :: Text -> [ Lexeme Token ]
lexText txt = lexer (initialInput "" txt)

-- | Parse a stream of lexemes.
specFromTokens :: [ Lexeme Token ] -> Either ParseError (Spec Parsed)
specFromTokens = runParser parseSpec . addLayoutTokens . filter notWhite
  where
  notWhite t = case lexemeToken t of
                 Skippable _ -> False
                 _           -> True

-- | Parse a text string.
specFromText :: Text -> Either ParseError (Spec Parsed)
specFromText = specFromTokens . lexText

-- | Parse a file. Raises an exception on parse error
-- (or problems reading the file).
specFromFile :: FilePath -> IO (Spec Parsed)
specFromFile file =
  do txt <- Text.readFile file
     case specFromText txt of
       Left err -> throwIO err
       Right a  -> return a

