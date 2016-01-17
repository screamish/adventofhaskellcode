{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import qualified Advent.Day2Spec as D2
import qualified Advent.Day3Spec as D3
import qualified Advent.Day4Spec as D4
import qualified Advent.Day5Spec as D5
import qualified Advent.Day6Spec as D6
import qualified Advent.Day7Spec as D7

main :: IO ()
main = hspec allSpecs

allSpecs :: Spec
allSpecs =
  sequence_ [
    D2.spec,
    D3.spec,
    -- These are expensive to run
    -- D4.spec,
    D5.spec,
    D6.spec,
    D7.spec
  ]
