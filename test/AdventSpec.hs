module Main where

import Test.Hspec
import Test.QuickCheck ()
import Advent.Day2
import Advent.Day3
import Advent.Day4
import Advent.Day5
import Advent.Day6 as D6
import Control.Arrow ((>>>))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day 6" $ do
--- Day 6: Probably a Fire Hazard ---

-- Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.
-- Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

    describe "parsing" $ do
      it "turn on 0,0 through 999,999 would turn on (or leave on) every light." $
        D6.parse "turn on 0,0 through 999,999" `shouldBe` Right [Command TurnOn ((0,0), (999,999))]

      it "toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off." $
        D6.parse "toggle 0,0 through 999,0" `shouldBe` Right [Command Toggle ((0,0), (999,0))]

      it "turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights." $
        D6.parse "turn off 499,499 through 500,500" `shouldBe` Right [Command TurnOff ((499,499), (500,500))]

      it "turn on 0,0 through 999,999 then turn off 499,499 through 500,500 should result in all but 4 lights being on" $
        D6.parse "turn on 0,0 through 999,999\nturn off 499,499 through 500,500" `shouldBe` Right [Command TurnOn ((0,0), (999,999)), Command TurnOff ((499,499), (500,500))]

    describe "running and counting" $ do
      let count = D6.parse >>> fmap (D6.runSteps D6.english False >>> D6.numberLit)
      it "turn on 0,0 through 999,999 would turn on (or leave on) every light." $
        count "turn on 0,0 through 999,999" `shouldBe` Right 1000000

      it "toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off." $
        count "toggle 0,0 through 999,0" `shouldBe` Right 1000

      it "turn on 0,0 through 999,999 then turn off 499,499 through 500,500 should result in all but 4 lights being on" $
        count "turn on 0,0 through 999,999\nturn off 499,499 through 500,500" `shouldBe` Right (1000000 - 4)

    describe "part b (elvish)" $ do
--       You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

-- The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

      it "The phrase turn on actually means that you should increase the brightness of those lights by 1." $ do
        D6.elvish TurnOn 1 `shouldBe` 2

      it "The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero." $ do
        D6.elvish TurnOff 1 `shouldBe` 0
        D6.elvish TurnOff 0 `shouldBe` 0

      it "The phrase toggle actually means that you should increase the brightness of those lights by 2." $ do
        D6.elvish Toggle 0 `shouldBe` 2
        D6.elvish Toggle 99 `shouldBe` 101
  
  describe "Day 5" $ do
    --- Day 5: Doesn't He Have Intern-Elves For This? ---
    -- Santa needs help figuring out which strings in his text file are naughty or nice.
    -- A nice string is one with all of the following properties:

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

    describe "part (b)" $ do

-- Now, a nice string is one with all of the following properties:

-- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
-- It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

      it "qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz)." $
         isNice2 "qjhvhtzxzqqjkmpb" `shouldBe` True

      it "xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap." $
         isNice2 "xxyxx" `shouldBe` True

      it "aaa is not nice because it has no overlapping pairs" $
         isNice2 "aaa" `shouldBe` False

      it "uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them." $
         isNice2 "uurcxstgmygtbstg" `shouldBe` False

      it "ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice." $
         isNice2 "ieodomkazucvgmuy" `shouldBe` False

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
