{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
-- import Data.Text as T
import Data.Char

type Name = String
data Wire = Wire Body Name deriving (Eq, Show, Ord)
data Arg = Name Name
         | ArgVal Int deriving (Eq, Show, Ord)
data Op = AND Arg Arg
        | OR Arg Arg deriving (Eq, Show, Ord)
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
      arg = (ArgVal <$> num) <|> (Name <$> name)
      op constructor identifier =
        Op <$> (constructor <$> arg <* token (word identifier) <*> arg) <?> identifier

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
