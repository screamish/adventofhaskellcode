module Advent.Day6 where

import qualified Data.Text as T
import Text.Parsec as P

data Command = TurnOn Bounds
             | TurnOff Bounds
             | Toggle Bounds
               deriving (Eq, Show)

type Index = (Int, Int)
type Start = Index
type End = Index
type Bounds = (Start, End)

parse :: T.Text -> Either ParseError [Command]
parse =
  P.parse commandP ""
  where
    commandP = many1 $ choice [on, tog, off]

    int = read <$> many1 digit
    index = (,) <$> int <*> (char ',' *> int)
    coords = (,) <$> index <*> (string " through " *> index)

    on  = try $ TurnOn <$ string "turn on " <*> coords
    off = try $ TurnOff <$ string "turn off " <*> coords
    tog = try $ Toggle <$ string "toggle " <*> coords
