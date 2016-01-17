module Main where

import qualified Data.Text.IO as IO
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as T
import Advent.Day2
import Advent.Day3
import Advent.Day4
import Advent.Day5
import qualified Advent.Day6 as D6
import qualified Advent.Day7 as D7
-- import Control.Monad (forM_)

main :: IO ()
main =
  day7

day7 :: IO ()
day7 = do
  input <- T.unpack <$> LIO.readFile "day7input.txt"
  -- let a = D7.parse (T.unpack input)
  -- forM_ a print
  let a = D7.eval1 (D7.toMap . D7.parse $ input) (D7.Name "a")
  print $ "Day 7(a): wire a = " ++ show a

  let a' = D7.feedbackLoop (D7.toMap . D7.parse $ input) (D7.Name "a") (D7.Name "b")
  print $ "Day 7(b): wire a = " ++ show a'

day6 :: IO ()
day6 = do
  input <- IO.readFile "day6input.txt"
  let totalLit = (D6.numberLit . D6.runSteps D6.english False) <$> D6.parse input
  print $ "Day 6(a): " ++ show totalLit ++ " lights lit"

  let brightness = (D6.totalBrightness . D6.runSteps D6.elvish 0) <$> D6.parse input
  print $ "Day 6(b): " ++ show brightness ++ " total brightness"

day5 :: IO ()
day5 = do
  input <- LIO.readFile "day5input.txt"
  let totalNice =
        length $ filter isNice $ T.lines input
  print $ "Day 5(a): " ++ show totalNice ++ " nice strings"

  let totalNice2 =
        length $ filter isNice2 $ T.lines input
  print $ "Day 5(b): " ++ show totalNice2 ++ " nice strings"

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

day4 :: IO ()
day4 = do
  let hashPart5 = hashPart 5 "bgvyzdsv"
  print $ "Day 4(a): " ++ show hashPart5 ++ " is the lowest number that can generate an MD5 hash with 5 leading zeroes"

  let hashPart6 = hashPart 6 "bgvyzdsv"
  print $ "Day 4(b): " ++ show hashPart6 ++ " is the lowest number that can generate an MD5 hash with 6 leading zeroes"
