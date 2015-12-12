module Main where

import qualified Data.Text.IO as IO
import Advent.Day2
import Advent.Day3
import Data.Either

main :: IO ()
main = do
  day2
  day3

day2 :: IO ()
day2 = do
  input <- IO.readFile "day2input.txt"
  let ribbon = calcRibbon input
  print $ "Day 2(b): " ++ show ribbon ++ " square feet required"

day3 :: IO ()
day3 = do
  input <- IO.readFile "day3input.txt"
  let visited = visitedHouses . visits <$> parseDirections input
  print $ "Day 3(a): " ++ printResult visited

  let visitedWithRobo = totalVisitedWithRobo <$> parseDirections input
  print $ "Day 3(b): " ++ printResult visitedWithRobo

  where printResult =
            either
              (const "parse error")
              (\t -> show t ++ " houses visited")
