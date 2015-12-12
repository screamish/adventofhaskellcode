module Main where

import Advent.Day2
import qualified Data.Text.IO as IO

main :: IO ()
main =
  day2

day2 :: IO ()
day2 = do
  input <- IO.readFile "day2input.txt"
  let ribbon = calcRibbon input
  print $ "Day 2(b): " ++ show ribbon ++ " square feet required"
