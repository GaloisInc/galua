{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Insert the special layout tokens needed.
module Galua.Spec.Parser.Layout (addLayoutTokens) where

import Galua.Spec.Parser.Lexer

open :: Int -> SourcePos -> Lexeme Token
open n p = Lexeme { lexemeToken = Open n
                  , lexemeText = ""
                  , lexemeRange = range p
                  }

indent :: Lexeme Token -> Lexeme Token
indent l = Lexeme { lexemeText  = ""
                  , lexemeToken = Indent (indentOf l)
                  , lexemeRange = range (startOf l)
                  }

startOf :: Lexeme Token -> SourcePos
startOf = sourceFrom . range

endOf :: Lexeme Token -> SourcePos
endOf = sourceTo . range

lineOf :: Lexeme Token -> Int
lineOf = sourceLine . startOf

indentOf :: Lexeme Token -> Int
indentOf = sourceColumn . startOf

-- | Assumes white-space tokens have been removed.
addLayoutTokens :: [ Lexeme Token ] -> [ Lexeme Token ]
addLayoutTokens ts =
  case ts of
    []     -> [ open 1 startPos ]
    x : xs ->
      case lexemeToken x of
        KW_open_brace -> x : goIndent x xs
        _             -> open (indentOf x) (startOf x) : goOpen (x : xs)


  where
  goOpen (a : b : more)
    | startsBlock a =
      case more of
        []     -> [ a, b, open 0 (endOf b) ]
        m : ms
          | KW_open_brace <- lexemeToken m -> a : b : m : goIndent m ms
          | otherwise -> a : b : open (indentOf m) (startOf m) : goOpen (m : ms)

  goOpen (a : more) = a : goIndent a more
  goOpen []         = []

  goIndent lst (a : more)
    | lineOf lst /= lineOf a = indent a : goOpen (a : more)
  goIndent _ xs = goOpen xs


startsBlock :: Lexeme Token -> Bool
startsBlock l = case lexemeToken l of
                  KW_class      -> True
                  KW_namespace  -> True
                  _             -> False


