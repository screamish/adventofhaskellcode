{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7 where

import Control.Applicative
import Text.Earley as E
import Data.Char
import Data.Word (Word16)
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Data.Bits ((.|.), (.&.), complement, shiftL, shiftR)
import Data.Maybe (fromMaybe)

newtype Name = Name { getName :: String } deriving (Eq, Ord)

instance Show Name where
  show = getName

type Val = Word16

data Wire = Wire Body Name deriving (Eq, Ord)

data Arg = ArgName Name
         | ArgVal Val
           deriving (Eq, Ord)

data Op = AND Arg Arg
        | OR Arg Arg
        | LSHIFT Arg Word16
        | RSHIFT Arg Word16
          deriving (Eq, Ord)

data Body = Val Val
          | Op Op
          | NOT Name
          | BName Name
            deriving (Eq, Ord)

eval' :: Map.Map Name Body -> Body -> State (Map.Map Name Val) Val
eval' env b =
  case b of
    Val v -> eval'' $ ArgVal v
    Op (AND a1 a2) -> (.&.) <$> eval'' a1 <*> eval'' a2
    Op (OR a1 a2) -> (.|.) <$> eval'' a1 <*> eval'' a2
    Op (LSHIFT a n) -> flip shiftL (fromIntegral n) <$> eval'' a
    Op (RSHIFT a n) -> flip shiftR (fromIntegral n) <$> eval'' a
    NOT n -> complement <$> eval'' (ArgName n)
    BName n -> eval'' $ ArgName n

  where
    eval'' :: Arg -> State (Map.Map Name Val) Val
    eval'' (ArgVal v) = return v
    eval'' (ArgName n) = do
      vs <- get
      case Map.lookup n vs of
        Just v -> return v
        Nothing -> do
          v <- eval' env $ env Map.! n
          modify $ Map.insert n v
          return v

wireG :: forall r. Grammar r (Prod r String Char Wire)
wireG = mdo
  whitespace <- rule $ many $ satisfy isSpace

  let token :: Prod r String Char a -> Prod r String Char a
      token p = whitespace *> p
      name :: Prod r String Char Name
      name = token $ Name <$> some (satisfy isAsciiLower) <?> "identifier"
      num :: (Num a, Read a) => Prod r String Char a
      num = token $ read <$> some (satisfy isDigit) <?> "number"
      arg = (ArgVal <$> num) <|> (ArgName <$> name)

  val <- rule $ Val <$> num <?> "Val"
  and <- rule $ Op <$> (AND <$> arg <* token (word "AND") <*> arg) <?> "AND"
  or <- rule $ Op <$> (OR <$> arg <* token (word "OR") <*> arg) <?> "OR"
  lshift <- rule $ Op <$> (LSHIFT <$> arg <* token (word "LSHIFT") <*> num) <?> "LSHIFT"
  rshift <- rule $ Op <$> (RSHIFT <$> arg <* token (word "RSHIFT") <*> num) <?> "LSHIFT"
  not <- rule $ NOT <$> (token (word "NOT") *> name) <?> "NOT"
  bname <- rule $ BName <$> name <?> "Name"
  body <- rule $ and <|> or <|> val <|> lshift <|> rshift <|> not <|> bname
  wire <- rule $ Wire <$> body <* token (word "->") <*> name <?> "Wire"
  return wire

parse :: String -> [Wire]
parse = join . fmap p . lines
  where
    p = fst . E.fullParses (E.parser wireG)

toMap :: [Wire] -> Map.Map Name Body
toMap wires = Map.fromList $ strip wires
  where strip = fmap (\(Wire b n) -> (n, b))

eval :: Map.Map Name Body -> Map.Map Name Val
eval wires = evalState (mapM (eval' wires) wires) mempty

eval1 :: Map.Map Name Body -> Name -> Val
eval1 wires wireName =
  fromMaybe (error $ "wireName: " ++ show wireName ++ " could not be found") $ Map.lookup wireName $ eval wires

feedbackLoop :: Map.Map Name Body -> Name -> Name -> Val
feedbackLoop wires wireName wireToFeedback =
  let initialOutput = eval1 wires wireName
      wires' = Map.update (const . Just . Val $ initialOutput) wireToFeedback wires
  in
    eval1 wires' wireName

-- Show instances for the DSL to support round-tripping via 'show . parse'

instance Show Wire where
  show (Wire body name) = show body ++ " -> " ++ show name

instance Show Arg where
  show a = case a of
    ArgName n -> show n
    ArgVal v -> show v

instance Show Body where
  show b = case b of
    Val v -> show v
    Op o -> show o
    NOT n -> "NOT " ++ show n
    BName n -> show n

instance Show Op where
  show o = case o of
    AND a b -> show a ++ " AND " ++ show b
    OR a b -> show a ++ " OR " ++ show b
    LSHIFT a n -> show a ++ " LSHIFT " ++ show n
    RSHIFT a n -> show a ++ " RSHIFT " ++ show n
