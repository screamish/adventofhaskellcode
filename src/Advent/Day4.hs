module Advent.Day4 where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.List
import Data.Maybe
import Data.Digest.Pure.MD5

hashPart :: Int -> T.Text -> Int
hashPart leadingZeroes seed =
  let
    makeCandidate i = (hashmd5 $ T.append seed $ T.pack $ show i, i)
    candidates = map makeCandidate [0..] in
  snd . fromJust $ find (match . fst) candidates
  where
    match = T.isPrefixOf $ T.replicate (fromIntegral leadingZeroes) "0"

hashmd5 :: T.Text -> T.Text
hashmd5 =
  T.pack . show . md5 . encodeUtf8
