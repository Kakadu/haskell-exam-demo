module Arithm where

import Text.Parsec
import Text.Parsec.Expr
import Data.Char (digitToInt)
import Text.Parsec.Expr
import ULC (stringConst)

data Term = Const Int
          | Plus Term Term
          | Mul  Term Term

eval :: Term -> Int
eval t = helper t id
  where
    helper (Const n) k = k n
    helper (Plus a b) k = helper a (\r1 -> helper b (\r2 -> k $ r1+r2))
    helper (Mul  a b) k = helper a (\r1 -> helper b (\r2 -> k $ r1*r2))

-- parsing stuff
decimal :: Parsec String a Term
decimal = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ Const n)

parser :: Parsec String a Term
parser =  buildExpressionParser table (spaces *> decimal <* spaces)
          <?> "aithmetic expression"

table =
  [ [binary "*" Mul  AssocLeft ]
  , [binary "+" Plus AssocLeft ]
  ]

binary name fun assoc = Infix (do{ stringConst name; return fun }) assoc

parse :: String -> Either ParseError Term
parse = runParser parser [] "untyped λ-calculus"
