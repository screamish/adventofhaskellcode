module Main where

import Advent.Day2
import qualified Data.Text.IO as IO

main :: IO ()
main = do
  input <- IO.readFile "day2input.txt"
  print $ calcRibbon input
