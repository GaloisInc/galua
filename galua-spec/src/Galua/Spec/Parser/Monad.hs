module Galua.Spec.Parser.Monad
  ( Parser
  , onMissingClose
  , lexerM
  , runParser
  , parseError
  , ParseError
  ) where

import           Control.Monad
import qualified Data.Text as Text
import           Control.Exception

import Galua.Spec.Parser.Lexer

import Debug.Trace


newtype Parser a = Parser (RW -> Either ParseError (a,RW))

newtype ParseError = ParseError String
                     deriving Show

instance Exception ParseError

data RW = RW { next          :: [Lexeme Token]
             , lastPos       :: SourcePos
             , tokens        :: [Lexeme Token]
             , layoutContext :: [Int]
             } deriving Show

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\s -> Right (a,s))
  (<*>)  = ap

instance Monad Parser where
  Parser m >>= f  = Parser (\s -> case m s of
                                    Left err -> Left err
                                    Right (a,s1) ->
                                       let Parser m1 = f a
                                       in m1 s1)

runParser :: Parser a -> [Lexeme Token] -> Either ParseError a
runParser (Parser m) ts = case m rw of
                            Left err    -> Left err
                            Right (a,_) -> Right a
  where
  rw = RW { next    = []
          , lastPos = SourcePos { sourceLine = 0
                                , sourceColumn = 0
                                , sourceIndex = -1 }
          , tokens = ts
          , layoutContext = []
          }

parseError :: Lexeme Token -> Parser a
parseError p = Parser (\_ ->
  let q = sourceFrom (range p)
  in Left $ ParseError $ "Parser error on line " ++ ppPos q)

ppPos :: SourcePos -> String
ppPos p = show (sourceLine p) ++ ":" ++ show (sourceColumn p)

onMissingClose :: Parser SourceRange
onMissingClose = Parser $ \rw ->
  case layoutContext rw of
    m : ms
      | m /= 0 -> Right ( range (lastPos rw)
                        , rw { next          = next rw
                             , layoutContext = ms
                             , tokens        = tokens rw })
    _ -> Left $ ParseError $ "Unterminated block at " ++ ppPos (lastPos rw)

lexerM :: (Lexeme Token -> Parser a) -> Parser a
lexerM k = k =<< nextToken

lx :: Char -> Token -> SourcePos -> Lexeme Token
lx c tok p = Lexeme { lexemeText  = Text.singleton c
                    , lexemeToken = tok
                    , lexemeRange = range p -- virtual do not take space
                    }

semi,open,close :: SourcePos -> Lexeme Token
semi  = lx ';' KW_semi
open  = lx '{' KW_open_brace
close = lx '}' KW_close_brace

nextToken :: Parser (Lexeme Token)
nextToken = Parser (\rw -> case next rw of
                             [] -> pick (lastPos rw)
                                        (tokens rw)
                                        (layoutContext rw)
                             p : ps -> Right (p, rw { next = ps
                                                    , lastPos = sourceTo (range p) }))
  where
  ok a ts ms = Right (a, RW { next  = []
                            , tokens = ts
                            , layoutContext = ms
                            , lastPos = sourceTo (range a)
                            })


  -- This is the same as in Haskell
  pick _ toks@(Lexeme { lexemeToken = Indent n, lexemeRange = p } : ts)
         ctxt@(m : ms)

    | m == n  = ok (semi (sourceTo p)) ts ctxt
    | n <  m  = ok (close (sourceTo p)) toks ms

  pick _ (Lexeme { lexemeToken = Indent _, lexemeRange = p } : ts) ms =
    pick (sourceTo p) ts ms

  pick _ (Lexeme { lexemeToken = Open n, lexemeRange = p } : ts) (m : ms)
    | n > m = ok (open (sourceTo p)) ts (n : m : ms)

  pick _ (Lexeme { lexemeToken = Open n, lexemeRange = p } : ts) [] =
    ok (open (sourceTo p)) ts [n]

  pick _ (Lexeme { lexemeToken = Open n, lexemeRange = p } : ts) ms =
    let loc = sourceTo p
    in Right ( open loc
             , RW { next          = [close loc]
                  , lastPos       = loc
                  , tokens        = Lexeme
                                      { lexemeText  = Text.pack ""
                                      , lexemeRange = range loc
                                      , lexemeToken = Indent n
                                      }
                                    : ts
                  , layoutContext = ms })

  pick _ (t@Lexeme { lexemeToken = KW_close_brace } : ts) (0 : ms) = ok t ts ms

  pick _ (Lexeme { lexemeToken = KW_close_brace, lexemeRange = r } : _) _ =
    Left $ ParseError $ "Unexpected end of block at " ++ show r

  pick _ (t@Lexeme { lexemeToken = KW_open_brace } : ts) ms = ok t ts (0 : ms)

  -- error case

  pick _ (t : ts) ms = ok t ts ms
  pick p [] []       = ok Lexeme { lexemeToken = EOF
                                 , lexemeText  = Text.pack ""
                                 , lexemeRange = range p
                                 } [] []

  pick p [] (m : ms)
    | m > 0     = ok (close p) [] ms
    | otherwise = Left $ ParseError "Unterminated block at the end of file."






