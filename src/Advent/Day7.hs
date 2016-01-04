{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
-- import Data.Text as T
import Data.Char

type Name = String
data Val = Val Int deriving (Eq, Show, Ord)
data Wire = Wire Val Name deriving (Eq, Show, Ord)

wireG :: Grammar r (Prod r String String Wire)
wireG = mdo
  -- x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x2
  --           <|> x2
  --           <?> "sum"
  -- arrow <- rule $
  val <- rule $ Val . read <$> (satisfy num <?> "number")
  wire <- rule $ Wire <$> val <* symbol "->" <*> (satisfy ident <?> "identifier")
  return wire
  where
    num = all isDigit
    ident = all isAlpha

parse :: String -> ([Wire], E.Report String String)
parse x = E.fullParses (E.parser wireG) $ lines x
