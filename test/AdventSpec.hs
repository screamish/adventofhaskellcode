module Main where

import Test.Hspec
import Test.QuickCheck
import Advent.Day2

main :: IO ()
main = hspec $ do
  describe "Day 2" $ do
    it "parses dimensions" $ do
      -- A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
      -- ribbonRequired "2x3x4" `shouldBe` Right 34
      parseDimensions "2x3x4" `shouldBe` Right (2, 3, 4)

    it "calculates ribbon #1" $ do
      ribbonForDimensions (2, 3, 4) `shouldBe` 34

    it "calculates ribbon #2" $ do
      ribbonForDimensions (1, 1, 10) `shouldBe` 14
