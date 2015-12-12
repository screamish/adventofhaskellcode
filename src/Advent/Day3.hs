module Advent.Day3 where

import qualified Data.Text as T
import Data.List
import Text.Parsec
import Control.Lens
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

visitedHouses :: [House] -> Int
visitedHouses =
  MultiSet.fromList
  >>> MultiSet.toOccurList
  >>> length

split :: [a] -> ([a], [a])
split xs =
  let ith = zip ([0..] :: [Int]) xs
      (l, r) = partition (\(i,_) -> i `mod` 2 == 0) ith
      strip = fmap snd
  in (strip l, strip r)

totalVisitedWithRobo :: [Direction] -> Int
totalVisitedWithRobo =
  split
  >>> (\(l, r) -> visits l ++ visits r)
  >>> visitedHouses
