module Advent.Day4Spec where

import Test.Hspec
import Advent.Day4

spec :: Spec
spec =
  describe "Day 4" $ do
    it "For input 'abcdef' the lowest number to generate a hash with 5 leading zeroes is 609043" $
      hashPart 5 "abcdef" `shouldBe` 609043

    it "For input 'pqrstuv' the lowest number to generate a hash with 5 leading zeroes is 1048970" $
      hashPart 5 "pqrstuv" `shouldBe` 1048970

    it "Can hash with MD5" $
      hashmd5 "abcdef609043" `shouldBe` "000001dbbfa3a5c83a2d506429c7b00e"
