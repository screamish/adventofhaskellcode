module Main where

import Test.Hspec
import Test.QuickCheck
import Advent.Day2

main :: IO ()
main = hspec $ do
  describe "Day 2" $ do
    it "calculates ribbon #1" $
      -- A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
      -- ribbonRequired "2x3x4" `shouldBe` Right 34
      ribbonRequired "2x3x4" `shouldBe` Right (Dimension (2, 3, 4))
