{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
-- import Data.Text as T
import Data.Char

type Name = String
data Wire = Wire Body Name deriving (Eq, Show, Ord)
data Op = AND Name Name
        | OR Name Name deriving (Eq, Show, Ord)
data Body = Val Int
          | Op Op deriving (Eq, Show, Ord)

wireG :: forall r. Grammar r (Prod r String Char Wire)
wireG = mdo
  whitespace <- rule $ many $ satisfy isSpace

  let token :: Prod r String Char a -> Prod r String Char a
      token p = whitespace *> p
      name :: Prod r String Char Name
      name = token $ some (satisfy isAsciiLower) <?> "identifier"
      num :: Prod r String Char Int
      num = token $ read <$> some (satisfy isDigit) <?> "number"
      op c i = Op <$> (c <$> name <* token (word i) <*> name) <?> i

  val <- rule $ Val <$> num <?> "Val"
  and <- rule $ op AND "AND"
  or <- rule $ op OR "OR"
  body <- rule $ and <|> or <|> val
  wire <- rule $ Wire <$> body <* token (word "->") <*> name <?> "Wire"
  return wire

parse :: String -> [Wire]
parse = fst . p
  where
    p = E.fullParses (E.parser wireG)
