module Advent.Day5
       ( isNice
       ) where

import qualified Data.Text.Lazy as T
import qualified Data.List as List
import qualified Data.Set as Set
import Control.Monad (liftM2)

vowels = Set.fromList "aeiou"
naughtySubStrings = ["ab", "cd", "pq", "xy"]
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
