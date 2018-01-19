module PMParser where

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token (reservedOp)
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)
import ULC (stringConst)

data Info = Info { row :: Int, col :: Int } deriving (Show)
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

type Parser rez = Parsec String () rez

data BinOpSort = Mul | Add deriving Show
data Ops a = TermConstruction
  { app   :: Info -> a -> a -> a
  , abs   :: Info -> String -> a  -> a
  , int   :: Info -> Int -> a
  , var   :: Info -> String -> a
  , reset :: Info -> a -> a
  , binop :: BinOpSort -> a -> a -> a
  }

decimal :: Ops a -> Parser a
decimal ops = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ int ops (infoFrom pos) n)

parseRhs ops =
  buildExpressionParser (table ops)
    (spaces *> (block ops) <* spaces)
  <?> "expression"
  where
    table ops =
      [ [binary "*" (binop ops Mul) AssocLeft ]
      , [binary "+" (binop ops Add) AssocLeft ]
      ]
    binary name fun assoc =
      Infix (do{ spaces *> (ULC.stringConst name); return fun }) assoc
    block ops = decimal ops
