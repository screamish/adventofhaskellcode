{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
import Data.Char
import Control.Monad (join)
import qualified Data.Map as Map

newtype Name = Name { getName :: String } deriving (Eq, Show, Ord)
type Val = Int

data Wire = Wire Body Name deriving (Eq, Show, Ord)

data Arg = ArgName Name
         | ArgVal Val
           deriving (Eq, Show, Ord)

data Op = AND Arg Arg
        | OR Arg Arg
        | LSHIFT Arg Int
        | RSHIFT Arg Int
          deriving (Eq, Show, Ord)

data Body = Val Val
          | Op Op
          | NOT Name
            deriving (Eq, Show, Ord)

wireG :: forall r. Grammar r (Prod r String Char Wire)
wireG = mdo
  whitespace <- rule $ many $ satisfy isSpace

  let token :: Prod r String Char a -> Prod r String Char a
      token p = whitespace *> p
      name :: Prod r String Char Name
      name = token $ Name <$> some (satisfy isAsciiLower) <?> "identifier"
      num :: Prod r String Char Int
      num = token $ read <$> some (satisfy isDigit) <?> "number"
      arg = (ArgVal <$> num) <|> (ArgName <$> name)

  val <- rule $ Val <$> num <?> "Val"
  and <- rule $ Op <$> (AND <$> arg <* token (word "AND") <*> arg) <?> "AND"
  or <- rule $ Op <$> (OR <$> arg <* token (word "OR") <*> arg) <?> "OR"
  lshift <- rule $ Op <$> (LSHIFT <$> arg <* token (word "LSHIFT") <*> num) <?> "LSHIFT"
  rshift <- rule $ Op <$> (RSHIFT <$> arg <* token (word "RSHIFT") <*> num) <?> "LSHIFT"
  not <- rule $ NOT <$> (token (word "NOT") *> name) <?> "NOT"
  body <- rule $ and <|> or <|> val <|> lshift <|> rshift <|> not
  wire <- rule $ Wire <$> body <* token (word "->") <*> name <?> "Wire"
  return wire

parse :: String -> [Wire]
parse = join . fmap p . lines
  where
    p = fst . E.fullParses (E.parser wireG)

eval :: String -> Map.Map Name Val
eval s = mempty
