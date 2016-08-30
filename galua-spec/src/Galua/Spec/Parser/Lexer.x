{
{-# LANGUAGE TemplateHaskell #-}
module Galua.Spec.Parser.Lexer
  ( lexer
  , Token(..)
  , Lexeme(..)
  , SourceRange(..)
  , SourcePos(..)
  , startPos
  , (<->)
  , HasRange(..)
  , Input(..), initialInput
  ) where

import AlexTools
}

$start_ident  = [a-zA-Z_]
$follow_ident = [a-zA-Z_0-9]


tokens :-
  "class"                     { lexeme KW_class               }
  "extends"                   { lexeme KW_extends             }
  "namespace"                 { lexeme KW_namespace           }
  "type"                      { lexeme KW_type                }
  "mutable"                   { lexeme KW_mutable             }

  "boolean"                   { lexeme KW_boolean             }
  "string"                    { lexeme KW_string              }
  "number"                    { lexeme KW_number              }
  "integer"                   { lexeme KW_integer             }
  "nil"                       { lexeme KW_nil                 }

  "?"                         { lexeme KW_quest               }
  "*"                         { lexeme KW_star                }
  ":"                         { lexeme KW_colon               }
  ";"                         { lexeme KW_semi                }
  "{"                         { lexeme KW_open_brace          }
  "}"                         { lexeme KW_close_brace         }
  "("                         { lexeme KW_open_paren          }
  ")"                         { lexeme KW_close_paren         }
  "="                         { lexeme KW_equals              }
  ","                         { lexeme KW_comma               }
  "->"                        { lexeme KW_arrow               }

  $start_ident $follow_ident* { lexeme Ident                  }

  "--" .* \n?                 { lexeme (Skippable Comment)    }
  $white+                     { lexeme (Skippable WhiteSpace) }

  .                           { lexeme Error                  }

{
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte = makeAlexGetByte $ \c -> fromIntegral (min 127 (fromEnum c))

lexer :: Input -> [Lexeme Token]
lexer = $makeLexer simpleLexer

data Token =
    KW_class
  | KW_extends
  | KW_namespace
  | KW_type
  | KW_mutable

  | KW_boolean
  | KW_string
  | KW_number
  | KW_integer
  | KW_nil

  | KW_quest
  | KW_star
  | KW_colon
  | KW_semi
  | KW_open_brace
  | KW_close_brace
  | KW_open_paren
  | KW_close_paren
  | KW_equals
  | KW_comma
  | KW_arrow

  | Ident
  | Skippable WhiteSpace
  | Error
  | EOF

  -- For layout
  | Indent !Int
  | Open   !Int
    deriving Show

data WhiteSpace = Comment | WhiteSpace
    deriving Show



}

