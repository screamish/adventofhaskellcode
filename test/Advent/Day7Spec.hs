{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7Spec where

import Test.Hspec
import Test.QuickCheck
import Advent.Day7 as D7
import Control.Monad (liftM)
import qualified Data.Map as Map

name :: String -> D7.Arg
name = ArgName . Name

arb :: Arbitrary a => Gen a
arb = arbitrary

instance Arbitrary D7.Arg where
  arbitrary = oneof [ArgName <$> arb, ArgVal <$> arb]

instance Arbitrary Op where
  arbitrary = oneof [AND <$> arb <*> arb
                    ,OR <$> arb <*> arb
                    ,LSHIFT <$> arb <*> arb
                    ,RSHIFT <$> arb <*> arb]

instance Arbitrary Body where
  arbitrary = oneof [liftM Val arbitrary, liftM Op arbitrary, liftM NOT arbitrary]

instance Arbitrary Name where
  arbitrary = do
    let chars = elements ['a'..'z']
    c <- chars
    cs <- listOf chars
    return $ Name (c : cs)

instance Arbitrary Wire where
  arbitrary = Wire <$> arbitrary <*> arbitrary

spec :: Spec
spec =
  describe "Day 7" $ do
    describe "parsing" $ do
      it "456 -> y" $
        D7.parse "456 -> y" `shouldBe` [Wire (Val 456) $ Name "y"]
      it "x AND y -> d" $
        D7.parse "x AND y -> d" `shouldBe` [Wire (Op (name "x" `AND` name "y")) $ Name "d"]
      it "1 AND y -> d" $
        D7.parse "1 AND y -> d" `shouldBe` [Wire (Op (ArgVal 1 `AND` name "y")) $ Name "d"]
      it "x OR y -> e" $
        D7.parse "x OR y -> e" `shouldBe` [Wire (Op (name "x" `OR` name "y")) $ Name "e"]
      it "x LSHIFT 2 -> f" $
        D7.parse "x LSHIFT 2 -> f" `shouldBe` [Wire (Op (name "x" `LSHIFT` 2)) $ Name "f"]
      it "y RSHIFT 2 -> g" $
        D7.parse "y RSHIFT 2 -> g" `shouldBe` [Wire (Op (name "y" `RSHIFT` 2)) $ Name "g"]
      it "NOT x -> h" $
        D7.parse "NOT x -> h" `shouldBe` [Wire (NOT (Name "x")) $ Name "h"]

      it "show and parse can round-trip" $ property $
        \(wire :: Wire) -> D7.parse (show wire) == [wire]

      it "this little bastard case" $
        D7.parse "lx -> a" `shouldBe` [Wire (BName . Name $ "lx") $ Name "a"]


      it "multiple wires over newlines" $ do
        let input = unlines
              ["123 -> x"
              ,"456 -> y"
              ,"NOT y -> i"]
        D7.parse input `shouldBe` [Wire (Val 123) $ Name "x"
                                  ,Wire (Val 456) $ Name "y"
                                  ,Wire (NOT (Name "y")) $ Name "i"]

    describe "evaluating" $
      it "scenario A" $ do
        let input = unlines
              ["123 -> x"
              ,"456 -> y"
              ,"x AND y -> d"
              ,"x OR y -> e"
              ,"x LSHIFT 2 -> f"
              ,"y RSHIFT 2 -> g"
              ,"NOT x -> h"
              ,"NOT y -> i"]
        (D7.eval . D7.toMap . D7.parse) input `shouldBe`
          Map.fromList [ (Name "d", 72)
                        ,(Name "e", 507)
                        ,(Name "f", 492)
                        ,(Name "g", 114)
                        ,(Name "h", 65412)
                        ,(Name "i", 65079)
                        ,(Name "x", 123)
                        ,(Name "y", 456)]
