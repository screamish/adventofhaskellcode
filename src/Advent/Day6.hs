module Advent.Day6 where

import qualified Data.Text as T
import Text.Parsec as P
import Data.Vector as V
import Control.Monad.State as ST

data Action = TurnOn
            | TurnOff
            | Toggle
              deriving (Eq, Show)

type Index = (Int, Int)
type Start = Index
type End = Index
type Bounds = (Start, End)
data Command = Command { _action :: Action, _bounds :: Bounds } deriving (Eq, Show)
type LightGrid = V.Vector (V.Vector Bool)

parse :: T.Text -> Either ParseError [Command]
parse =
  P.parse commandP ""
  where
    commandP = many1 $ command

    int = read <$> many1 digit
    index = (,) <$> int <*> (char ',' *> int)
    coords = (,) <$> index <*> (string " through " *> index)

    on  = try $ TurnOn <$ string "turn on "
    off = try $ TurnOff <$ string "turn off "
    tog = try $ Toggle <$ string "toggle "
    command = Command <$> choice [on, tog, off] <*> coords

initialState :: LightGrid
initialState = V.replicate 1000 $ V.replicate 1000 False



-- step :: Command -> State LightGrid ()
-- step c =
--   ST.modify (m)
--   where
--     m grid = 
--     delta (TurnOn _) = const True
--     delta (TurnOff _) = const False
--     delta (Toggle _) = not
