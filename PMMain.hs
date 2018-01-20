module Main where

import PMParser

data Expr =
    EConst Int
  | TmApp  Expr Expr
  | EConstr String [Expr]
  | ETag String
  | EField Int String
  | EBinOp BinOpSort Expr Expr
  | EVar   String
  deriving (Show)

optsE = TermConstruction
        { var  = const EVar
        , int   = const EConst
        , field = const EField
        , tag   = const ETag
        , binop = EBinOp
        , econstr = const EConstr
        }
data Patt = PWild | PVar String | PConstr String [Patt]
  deriving (Show)

optsP = PattConstruction
  { wild    = const PWild
  , pconstr = const PConstr
  , named   = const PVar
  }

rhs = [ "1+2*3", "x+y*3", "1+(field 1 x)+3", "(tag x) * (field 18 y)" ]
patts = [ "C _", "_", "C(x,y)", "C(D,E(x))", "U(_)" ]
cases = [ "_ -> 1", "x -> x"]
main = do
  print "hello"
  mapM_ (\line -> print line >> print (PMParser.parseRhs  optsE    line))
    rhs
  mapM_ (\line -> print line >> print (PMParser.parsePatt optsP    line))
    patts
  mapM_ (\line -> print line >> print (PMParser.parseCase optsP optsE line))
    cases
