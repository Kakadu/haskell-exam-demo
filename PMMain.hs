module Main where

import PMParser

ops :: Ops Term
ops = TermConstruction
              { var  = const TmVar
              , app  = const TmApp
              , int   = const TmConst
              , binop = TmBinOp
              }

rhs = ["1+2*3"]
main = do
  print "hello"
  mapM_ (\line -> print line >> print (PMParser.parseRhs ops line)) rhs
