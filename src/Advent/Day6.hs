{-# LANGUAGE FlexibleContexts #-}

module Advent.Day6 where

import qualified Data.Text as T
import Text.Parsec as Parsec (many1
                            , char
                            , newline
                            , sepEndBy
                            , try
                            , string
                            , ParseError
                            , digit
                            , choice
                            , parse)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, lift, ask)
import Control.Monad (forM)

data Action = TurnOn
            | TurnOff
            | Toggle
              deriving (Eq, Show)

type Index = (Int, Int)
type Start = Index
type End = Index
type Bounds = (Start, End)
data Command = Command { _action :: Action, _bounds :: Bounds } deriving (Eq, Show)
type LightGridM s a = V.Vector (VM.MVector s a)
type LightGrid a = V.Vector (V.Vector a)

type MyMon s a = ReaderT (LightGridM s a) (ST s)

parse :: T.Text -> Either ParseError [Command]
parse =
  Parsec.parse commandP ""
  where
    commandP = sepEndBy command newline

    int = read <$> many1 digit
    index = (,) <$> int <*> (char ',' *> int)
    coords = (,) <$> index <*> (string " through " *> index)

    on  = try $ TurnOn <$ string "turn on "
    off = try $ TurnOff <$ string "turn off "
    tog = try $ Toggle <$ string "toggle "
    command = Command <$> choice [on, tog, off] <*> coords

initialState :: a -> ST s (LightGridM s a)
initialState initialValue = sequence $ V.replicate 1000 inner
  where
    inner = VM.replicate 1000 initialValue

elvish :: Action -> Int -> Int
elvish TurnOn = (+ 1)
elvish TurnOff = max 0 . subtract 1
elvish Toggle = (+ 2)

english :: Action -> Bool -> Bool
english TurnOn = const True
english TurnOff = const False
english Toggle = not

reifyBounds :: Bounds -> [Index]
reifyBounds ((x1,y1), (x2,y2)) = [(x,y) | x <- [x1 .. x2], y <- [y1 .. y2]]

-- | I have no real idea how performant this is. I started going down the rabbit
-- hole of mutable vectors inside a monad stack and this is where I ended up.
step :: (Action -> a -> a) -> Command -> MyMon s a ()
step language c =
  mapM_ update indices
  where
    update (x,y) = do
        grid <- ask
        let ys = (V.!) grid y
        -- No idea why I can't reference modify, it's there on Hackage :S
        -- so have to do it the verbose way
        -- lift $ Data.Vector.Mutable.modify ys f x
        v <- lift $ VM.read ys x
        lift $ VM.write ys x (setter v)
    setter = language $ _action c
    indices = reifyBounds $ _bounds c


runSteps :: (Action -> a -> a) -> a -> [Command] -> LightGrid a
runSteps language initialValue t = runST $ do
    s <- initialState initialValue
    _ <- runReaderT (Control.Monad.forM t $ step language) s
    freeze s

freeze :: LightGridM s a -> ST s (LightGrid a)
freeze = sequence . fmap V.freeze

numberLit :: LightGrid Bool -> Int
numberLit =
  V.sum . fmap rows
  where
    rows :: V.Vector Bool -> Int
    rows = length . V.filter id

totalBrightness :: LightGrid Int -> Int
totalBrightness = V.sum . fmap V.sum
