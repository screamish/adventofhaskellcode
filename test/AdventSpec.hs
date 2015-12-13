module Main where

import Test.Hspec
import Test.QuickCheck
import Advent.Day2
import Advent.Day3
import Advent.Day4
import Advent.Day5
import qualified Data.Text.Lazy as T
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "Day 5" $ do
    --- Day 5: Doesn't He Have Intern-Elves For This? ---
    -- Santa needs help figuring out which strings in his text file are naughty or nice.
    -- A nice string is one with all of the following properties:

    it "contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou." $ property $ \s ->
       let vowels = Set.fromList ("aeiou" :: String)
           t = T.pack s
       in
         isNice t ==> T.length (T.filter (`Set.member` vowels) t) >= 3

    it "contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd)." $ property $ \s ->
      let t = T.pack s
      in isNice t ==> length (filter ((> 1) . T.length) (T.group t)) > 0

    it "does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements." $ property $ \s ->
      let t = T.pack s
          naughtySubStrings = ["ab", "cd", "pq", "xy"]
      in isNice t ==> not (any (`T.isInfixOf` t) naughtySubStrings)

    describe "nice strings" $ do
      it "ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings." $
        isNice "ugknbfddgicrmopn" `shouldBe` True

      it "aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap." $
        isNice "aaa" `shouldBe` True

    describe "naughty strings" $ do
      it "jchzalrnumimnmhp is naughty because it has no double letter." $
        isNice "jchzalrnumimnmhp" `shouldBe` False

      it "haegwjzuvuyypxyu is naughty because it contains the string xy." $
        isNice "haegwjzuvuyypxyu" `shouldBe` False

      it "dvszwmarrgswjxmb is naughty because it contains only one vowel." $
        isNice "dvszwmarrgswjxmb" `shouldBe` False

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

expensiveSpecs :: Spec
expensiveSpecs =
  describe "Day 4" $ do
    it "For input 'abcdef' the lowest number to generate a hash with 5 leading zeroes is 609043" $
      hashPart 5 "abcdef" `shouldBe` 609043

    it "For input 'pqrstuv' the lowest number to generate a hash with 5 leading zeroes is 1048970" $
      hashPart 5 "pqrstuv" `shouldBe` 1048970

    it "Can hash with MD5" $
      hashmd5 "abcdef609043" `shouldBe` "000001dbbfa3a5c83a2d506429c7b00e"
