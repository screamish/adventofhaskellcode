module Advent.Day4 where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.List
import Data.Maybe
import Text.Parsec
import Control.Lens
import Control.Arrow ((>>>))
import Data.Digest.Pure.MD5

hashPart :: T.Text -> Int
hashPart seed =
  let
    makeCandidate i = (hashmd5 $ T.append seed $ T.pack $ show i, i)
    candidates = map makeCandidate [0..] in
  snd . fromJust $ find (match . fst) candidates
  where
    match = T.isPrefixOf "00000"

hashmd5 :: T.Text -> T.Text
hashmd5 =
  T.pack . show . md5 . encodeUtf8
