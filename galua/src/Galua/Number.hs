{-# LANGUAGE DeriveGeneric #-}

module Galua.Number where

import           Control.Applicative
import           Control.Monad (guard,when)
import           Data.Bits (shiftL,shift)
import           Data.Char (isDigit, isHexDigit, digitToInt)
import           Data.Hashable(Hashable(..))
import           GHC.Generics
import           Numeric (readInt)
import           Text.Read (readMaybe)
import qualified Text.ParserCombinators.ReadP as RP

import qualified Numeric.Extras as N
import           CMath

data Number
  = Int    {-# UNPACK #-} !Int
  | Double {-# UNPACK #-} !Double
  deriving (Show, Read, Generic)

instance Ord Number where
  Int    x <  Int    y = x < y
  Int    x <  Double y = ltIntFloat x y
  Double x <  Double y = x < y
  Double x <  Int    y = not (isNaN x || leIntFloat y x)

  Int    x <= Int    y = x <= y
  Int    x <= Double y = leIntFloat x y
  Double x <= Double y = x <= y
  Double x <= Int    y = not (isNaN x || ltIntFloat y x)

instance Eq Number where
  Int     x   == Int     y = x == y
  Double  x   == Double  y = x == y
  Int     x   == Double  y = Just x == doubleAsInt y
  Double  x   == Int     y = doubleAsInt x == Just y

instance Hashable Number where
  hashWithSalt s n =
    case n of
      Int x    -> con 1 x
      Double x -> con 2 x
    where con i a = hashWithSalt (hashWithSalt s (i::Int)) a

------------------------------------------------------------------------
-- Casts
------------------------------------------------------------------------

liftDoubleOp :: (Double -> Double -> Double) -> Number -> Number -> Number
liftDoubleOp (?) x y = Double (numberToDouble x ? numberToDouble y)
{-# INLINE liftDoubleOp #-}

instance Num Number where
  Int x + Int y = Int (x+y)
  x     + y     = liftDoubleOp (+) x y

  Int x - Int y = Int (x-y)
  x     - y     = liftDoubleOp (-) x y

  Int x * Int y = Int (x*y)
  x     * y     = liftDoubleOp (*) x y

  negate (Int    x) = Int    (negate x)
  negate (Double x) = Double (negate x)

  abs (Int    x) = Int    (abs x)
  abs (Double x) = Double (abs x)

  signum (Int    x) = Int (signum x)
  signum (Double x) = Double (signum x) -- Int?

  fromInteger x
    | fromIntegral (minBound :: Int) <= x
    , fromIntegral (maxBound :: Int) >= x = Int (fromIntegral x)
    | otherwise                           = Double (fromIntegral x)

numberPow :: Number -> Number -> Number
numberPow = liftDoubleOp (**)

numberMod :: Number -> Number -> Number
numberMod (Int x) (Int y) = Int (mod x y)
numberMod x y = liftDoubleOp nummod x y

nummod :: Double -> Double -> Double
nummod x y
  | m*y < 0 = m+y
  | otherwise = m
  where
  m = N.fmod x y

numberDiv :: Number -> Number -> Number
numberDiv  = liftDoubleOp (/)

numberIDiv :: Number -> Number -> Number
numberIDiv (Int x) (Int y)
  | x == minBound && y == (-1) = Int x
  | otherwise                  = Int (div x y)
numberIDiv x y = liftDoubleOp (\a b -> N.floor (a/b)) x y

------------------------------------------------------------------------
-- Casts
------------------------------------------------------------------------

stringToNumber :: String -> Maybe Number
stringToNumber x = Double <$> readMaybe x -- XXX: wrong number format

numberToString :: Number -> String
numberToString (Int x) = show x
numberToString (Double x) = show x -- XXX: wrong number format

leIntFloat :: Int -> Double -> Bool
leIntFloat i d
  | intFitsDouble i                    = fromIntegral i <= d
  | -fromIntegral (minBound::Int) <= d = True
  |  fromIntegral (minBound::Int) <= d = i <= truncate d
  | otherwise                          = False

ltIntFloat :: Int -> Double -> Bool
ltIntFloat i d
  | intFitsDouble i                    = fromIntegral i < d
  | -fromIntegral (minBound::Int) <= d = True
  |  fromIntegral (minBound::Int) <  d = i < truncate d
  | otherwise                          = False

intFitsDouble :: Int -> Bool
intFitsDouble x = -(1`shiftL`53) <= x && x <= (1`shiftL`53)

numberToInt :: Number -> Maybe Int
numberToInt (Int    i) = Just i
numberToInt (Double d) = doubleAsInt d

numberToDouble :: Number -> Double
numberToDouble (Int    i) = fromIntegral i
numberToDouble (Double d) = d

doubleAsInt :: Double -> Maybe Int
doubleAsInt x
  | N.floor x == x -- Lua tests for integerness with floor
  , x >=  fromIntegral (minBound :: Int)
  , x < - fromIntegral (minBound :: Int)
  = Just (truncate x) -- Lua converts with truncate
  | otherwise = Nothing

numberFloor :: Number -> Number
numberFloor (Int n) = Int n
numberFloor (Double d) =
  let d_ = N.floor d in
  case doubleAsInt d_ of
    Nothing -> Double d_
    Just i  -> Int i

numberCeil :: Number -> Number
numberCeil (Int n) = Int n
numberCeil (Double d) =
  let d_ = N.ceil d in
  case doubleAsInt d_ of
    Nothing -> Double d_
    Just i  -> Int i

numberModf :: Number -> (Number, Number)
numberModf (Int x) = (Int x, Double 0)
numberModf (Double d) = (whole', Double frac)
  where
  (whole,frac) = N.modf d
  whole' = case doubleAsInt whole of
             Nothing -> Double whole
             Just i  -> Int i

-- 3.0     3.1416     314.16e-2     0.31416E1     34e1
-- 0x0.1E  0xA23p-4   0X1.921FB54442D18P+1

parseNumber :: String -> Maybe Number
parseNumber s =
  case RP.readP_to_S (parseFloatP s) s of
    [(n,_)] -> Just n
    _       -> Nothing

psign :: RP.ReadP (Int -> Int)
psign = id <$ RP.char '+'
    <|> negate <$ RP.char '-'
    <|> pure id

parseFloatP :: String -> RP.ReadP Number
parseFloatP original =
  do RP.skipSpaces

     sgn <- psign

     hex <- True <$ RP.string "0x"
        <|> True <$ RP.string "0X"
        <|> pure False

     let p    = if hex then isHexDigit else isDigit
         base = if hex then 16         else 10

     xs <- RP.munch p

     hasPoint <- True <$ RP.char '.'
             <|> pure False

     ys <- RP.munch p

     guard (not (null xs && null ys))

     hasExp <-
            True <$ (if hex then RP.char 'p' <|> RP.char 'P'
                            else RP.char 'e' <|> RP.char 'E')
           <|> pure False

     when hasExp $
       do _ <- psign
          _ <- RP.munch1 isDigit
          return ()

     RP.skipSpaces
     RP.eof

     let d | hasPoint || hasExp = Double (libc_strtod original)
           | otherwise = Int (sgn (fst (head (readInt base p digitToInt xs))))
     return d

forceDouble :: Number -> Number
forceDouble (Double d) = Double d
forceDouble (Int i) = Double (fromIntegral i)


--------------------------------------------------------------------------------

wordshiftL :: Int -> Int -> Int
wordshiftL x y = fromIntegral ((fromIntegral x :: Word) `shift` y)

wordshiftR :: Int -> Int -> Int
wordshiftR x y = wordshiftL x (negate y)


