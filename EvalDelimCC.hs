module EvalDelimCC where

import Prelude hiding (abs)
import ULC

data Term =
    TmVar  String
  | TmAbs  String Term
  | TmApp  Term Term
  | TmReset  Term
  | TmCallCC  String Term
  | TmBinOp BinOpSort Term Term
  | TmConst Int
  deriving (Show)

ops :: Ops Term
ops = TermConstruction
              { abs  = const TmAbs
              , var  = const TmVar
              , app  = const TmApp
              , reset = const TmReset
              , call  = const TmCallCC
              , int   = const TmConst
              , binop = TmBinOp
              }

parse = ULC.parse2 ops


eval :: Term -> Maybe Int
eval _ = Just 42
