module Advent.Day5Spec where

import Test.Hspec
import Advent.Day5

spec :: Spec
spec =
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
