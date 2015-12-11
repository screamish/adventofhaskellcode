module Advent.Day2 where

import Data.Text
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

data Dimension = Dimension (Int, Int, Int)
                 deriving (Show, Eq)

ribbonRequired :: Text -> Either ParseError Dimension
ribbonRequired dim =
  parse dimensions "" dim
  where
    dimensions = do
      x <- int
      char 'x'
      y <- int
      char 'x'
      z <- int
      return $ Dimension (x,y,z)

    lexer = makeTokenParser emptyDef
    int :: Parser Int
    int = read <$> many1 digit
