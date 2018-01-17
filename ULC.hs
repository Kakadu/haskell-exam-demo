module ULC where
-- changed after adoption  from
-- https://github.com/wetmore/TAPL-implementations/blob/master/untyped/Parser.hs

import Prelude hiding (abs)
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token (reservedOp)
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)

-- data Term =
--     TmVar Info Int Int
--   | TmAbs Info String Term
--   | TmApp Info Term Term
--   deriving (Show)

data Info = Info { row :: Int, col :: Int } deriving (Show)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

type BoundContext = [String]
type LCParser rez = Parsec String BoundContext rez

parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

stringConst :: String -> Parsec String u ()
stringConst [] = return ()
stringConst (c:cs) = do
  char c
  stringConst cs

data BinOpSort = Mul | Add deriving Show
data Ops a = Record
  { app   :: Info -> a -> a -> a
  , abs   :: Info -> String -> a  -> a
  , int   :: Info -> Int -> a
  , var   :: Info -> String -> a
  , reset :: Info -> a -> a
  , call  :: Info -> String -> a -> a   -- a.k.a. shift
  , binop :: BinOpSort -> a -> a -> a
  }

parseVar :: Ops a -> LCParser a
parseVar ops = do
  v <- parseVarName
  list <- getState
  findVar v list
  where
    --findVar :: String -> BoundContext -> LCParser a
    findVar v list = case elemIndex v list of
      Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
      Just n  -> do
        pos <- getPosition
        return $ var ops (infoFrom pos) v

parseAbs :: Ops a -> LCParser a
parseAbs ops = do
  char '\\' <|> char 'λ'
  v <- parseVarName
  modifyState (v :)
  char '.'
  term <- parseTerm ops
  modifyState tail
  pos <- getPosition
  return $ abs ops (infoFrom pos) v term

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseReset :: Ops a -> LCParser a
parseReset ops = do
  space
  stringConst "reset"
  space
  t <- parseTerm ops
  pos <- getPosition
  return $ reset ops (infoFrom pos) t

decimal :: Ops a -> LCParser a
decimal ops = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ int ops (infoFrom pos) n)

parseTerm :: Ops a -> LCParser a
parseTerm ops =
  chainl1 parseNonApp $ do
    space
    pos <- getPosition
    return $ app ops (infoFrom pos)
  where
    --parseNonApp :: LCParser a
    parseNonApp = parens (parseTerm ops) -- (M)
               <|> parseAbs ops          -- λx.M
               <|> parseVar ops          -- x
               <|> parseReset ops        -- reset T
               <|> decimal ops           -- 5
               <|> root ops

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped λ-calculus"

parse :: Ops a -> String -> Either ParseError a
parse ops = parseWith $ parseTerm ops
-- --------------------------------------
parse2 ops = parseWith $ root ops


root ops =  buildExpressionParser (table ops) (spaces *> (term ops) <* spaces)
            <?> "expression"
  where
    term ops =  parens (root ops)  -- (M)
            <|> parseAbs ops       -- λx.M
            <|> parseVar ops       -- x
            <|> decimal ops        -- 5
    parseAbs ops = do
      char '\\' <|> char 'λ'
      v <- parseVarName
      modifyState (v :)
      spaces
      char '.'
      spaces
      term <- root ops
      modifyState tail
      pos <- getPosition
      return $ abs ops (infoFrom pos) v term
    parseVar :: Ops a -> LCParser a
    parseVar ops = do
      v <- parseVarName
      list <- getState
      findVar v list
      where
        --findVar :: String -> BoundContext -> LCParser a
        findVar v list = case elemIndex v list of
          Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
          Just n  -> do
            pos <- getPosition
            return $ var ops (infoFrom pos) v

table ops =
  [ [binary "*" (binop ops Mul) AssocLeft ]
  , [binary "+" (binop ops Add) AssocLeft ]
  ]

binary name fun assoc = Infix (do{ stringConst name; return fun }) assoc
--prefix  name fun       = Prefix (do{ reservedOp name; return fun })
--postfix name fun       = Postfix (do{ reservedOp name; return fun })