{-# LANGUAGE RecursiveDo #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
-- import Data.Text as T
import Data.Char

type Name = String
data Val = Val Int deriving (Eq, Show, Ord)
data Wire = Wire Val Name deriving (Eq, Show, Ord)

wireG :: E.Grammar r (E.Prod r String String Wire) a
wireG = mdo
  -- x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x2
  --           <|> x2
  --           <?> "sum"
  -- arrow <- rule $
  wire <- rule $ Wire <$> val <* namedSymbol "->" <*> (satisfy ident <?> "identifier")
  val <- rule $ Val . read <$> (satisfy num <?> "number")
  return wire
  where
    num = all isDigit
    ident = all isAlpha

-- parse :: String -> Wire
-- parse :: String -> ([Wire], E.Report Expected String)
-- parse x = E.fullParses (E.parser wireG) $ words x
