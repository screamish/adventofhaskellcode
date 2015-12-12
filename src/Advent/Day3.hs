module Advent.Day3 where

import qualified Data.Text as T
import Data.List
import Data.Either (rights)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Lens
import Control.Lens.Tuple
import qualified Data.MultiSet as MultiSet
import Control.Arrow ((>>>))

data Direction = North | South | East | West deriving (Eq, Show)
type House = (Int, Int)

parseDirections :: T.Text -> Either ParseError [Direction]
parseDirections =
  parse directionsP ""
  where
    directionsP = many1 $ choice [n, s, e, w]

    n = North <$ char '^'
    s = South <$ char 'v'
    e = East <$ char '>'
    w = West <$ char '<'

totalDelivered :: [Direction] -> Int
totalDelivered = length . visits

visits :: [Direction] -> [House]
visits =
  scanl move (0,0)
  where
    move = flip delta
    delta North = _2 +~ 1
    delta South = _2 +~ -1
    delta West = _1 +~ -1
    delta East = _1 +~ 1

visitedHouses :: [Direction] -> Int
visitedHouses =
  visits
  >>> MultiSet.fromList
  >>> MultiSet.toOccurList
  >>> length
