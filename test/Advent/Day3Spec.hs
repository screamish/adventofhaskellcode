module Advent.Day3Spec where

import Test.Hspec
import Advent.Day3

spec :: Spec
spec =
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
