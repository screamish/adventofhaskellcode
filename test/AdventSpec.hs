module Main where

import Test.Hspec
import Test.QuickCheck
import Advent.Day2
import Advent.Day3

main :: IO ()
main = hspec $ do
  describe "Day 2" $ do
    it "parses dimensions" $
      -- A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
      -- ribbonRequired "2x3x4" `shouldBe` Right 34
      parseDimensions "2x3x4" `shouldBe` Right (2, 3, 4)

    it "calculates ribbon #1" $
      ribbonForDimensions (2, 3, 4) `shouldBe` 34

    it "calculates ribbon #2" $
      ribbonForDimensions (1, 1, 10) `shouldBe` 14

  describe "Day 3" $ do
    it "parses directions" $
      parseDirections "^>>v<" `shouldBe` Right [North, East, East, South, West]

    describe "The case '>'" $
      let dir = [East] in do
      it "delivers 2 presents" $
        totalDelivered dir `shouldBe` 2
      it "visits these houses" $
        visits dir `shouldBe` [(0,0), (1,0)]
      it "visits 2 houses" $
        (visitedHouses . visits) dir `shouldBe` 2

    describe "The case '^>v<'" $
      let dir = [North, East, South, West] in do
      it "delivers 5 presents" $
        totalDelivered dir `shouldBe` 5
      it "visits these houses" $
        visits dir `shouldBe` [(0,0), (0,1), (1,1), (1,0), (0,0)]
      it "visits 4 houses" $
        (visitedHouses . visits) dir `shouldBe` 4

    describe "The case '^v^v^v^v^v'" $
      let dir = (take 10 . cycle $ [North, South]) in do
      it "delivers 11 presents" $
        totalDelivered dir `shouldBe` 11
      it "visits these houses" $
        visits dir `shouldBe` (take 11 . cycle) [(0,0), (0,1)]
      it "visits 2 houses" $
        (visitedHouses . visits) dir `shouldBe` 2

    describe "RoboSanta" $ do
      it "^v visits 3 houses" $
        totalVisitedWithRobo [North, South] `shouldBe` 3
      it "^>v< visits 3 houses" $
        totalVisitedWithRobo [North, East, South, West] `shouldBe` 3
      it "^v^v^v^v^v visits 11 houses" $
        let dir = (take 10 . cycle $ [North, South]) in
        totalVisitedWithRobo dir `shouldBe` 11
