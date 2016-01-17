module Advent.Day6Spec where

import Test.Hspec
import Advent.Day6 as D6
import Control.Arrow ((>>>))

spec :: Spec
spec =
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

      it "The phrase turn on actually means that you should increase the brightness of those lights by 1." $
        D6.elvish TurnOn 1 `shouldBe` 2

      it "The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero." $ do
        D6.elvish TurnOff 1 `shouldBe` 0
        D6.elvish TurnOff 0 `shouldBe` 0

      it "The phrase toggle actually means that you should increase the brightness of those lights by 2." $ do
        D6.elvish Toggle 0 `shouldBe` 2
        D6.elvish Toggle 99 `shouldBe` 101
