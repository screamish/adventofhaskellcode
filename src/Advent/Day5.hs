module Advent.Day5
       ( isNice
       , isNice2
       ) where

import qualified Data.Text.Lazy as T
import qualified Data.Set as Set
import Data.List
import Control.Monad (liftM2)

vowels :: Set.Set Char
vowels = Set.fromList "aeiou"

naughtySubStrings :: [T.Text]
naughtySubStrings = ["ab", "cd", "pq", "xy"]

(.&&.) :: Monad m => m Bool -> m Bool -> m Bool
(.&&.) = liftM2 (&&)

isNice :: T.Text -> Bool
isNice =
  atLeast3Vowels
  .&&. atLeast1DoubleLetter
  .&&. noNaughtySubstrings
  where
    atLeast3Vowels t = T.length (T.filter (`Set.member` vowels) t) >= 3
    atLeast1DoubleLetter t = any ((> 1) . T.length) (T.group t)
    noNaughtySubstrings t = not (any (`T.isInfixOf` t) naughtySubStrings)

isNice2 :: T.Text -> Bool
isNice2 =
  ((repeatedPair . pairs) .&&. repeatedWithSingleGap) . T.unpack
  where
    -- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
    repeatedPair [] = False
    repeatedPair [_] = False
    repeatedPair [_,_] = False
    repeatedPair ( x:y:xs ) = x `elem` xs || repeatedPair (y:xs)
    pairs [] = []
    pairs [_] = []
    pairs xs = take 2 xs : (pairs . tail) xs

    -- It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
    repeatedWithSingleGap [] = False
    repeatedWithSingleGap [_] = False
    repeatedWithSingleGap [_,_] = False
    repeatedWithSingleGap ( x:y:z:rest ) = x == z || repeatedWithSingleGap (y:z:rest)
