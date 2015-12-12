module Advent.Day2 where

import qualified Data.Text as T
import Data.List
import Data.Either (rights)
import Text.Parsec
import Text.Parsec.Text (Parser)

type Dimensions = (Int, Int, Int)

parseDimensions :: T.Text -> Either ParseError Dimensions
parseDimensions dim =
  parse dimensions "" dim
  where
    dimensions = do
      x <- int
      char 'x'
      y <- int
      char 'x'
      z <- int
      return (x,y,z)

    int :: Parser Int
    int = read <$> many1 digit

ribbonForDimensions :: Dimensions -> Int
ribbonForDimensions (x, y, z) =
  bow + wrap
  where
    wrap = sum (fmap (* 2) . take 2 $ sort [x,y,z])
    bow = x * y * z

calcRibbon :: T.Text -> Int
calcRibbon input =
  sum $ rights $ fmap parse $ lines
  where
    lines = T.lines input
    parse x = fmap ribbonForDimensions $ parseDimensions x
