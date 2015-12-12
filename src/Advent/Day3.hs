module Advent.Day3 where

import qualified Data.Text as T
import Data.List
import Data.Either (rights)
-- import Data.Either.Combinators (fromRight)
import Text.Parsec
import Text.Parsec.Text (Parser)

data Direction = North | South | East | West deriving (Eq, Show)
type House = (Int, Int)

parseDirections :: T.Text -> Either ParseError [Direction]
parseDirections =
  parse directions ""
  where
    directions = many1 $ choice [n, s, e, w]

    n = char '^' >> pure North
    s = char 'v' >> pure South
    e = char '>' >> pure East
    w = char '<' >> pure West

totalDelivered :: [Direction] -> Int
totalDelivered d =
  length d + 1
