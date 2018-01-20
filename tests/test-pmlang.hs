module Main where

import PMParser (parseScrutinee, parseCase)
import Test.HUnit
import Data.Either
import System.Exit
import PMEval --(opsE, opsP, eval, EvalRez)

test1 name what casesS expected =
  TestLabel name $ TestList $ [c1] ++ c2 ++ [c3]
  where
    w = PMParser.parseScrutinee opsE what
    c1 = TestCase (assertBool "parsable" (isRight w))
    cases = map (PMParser.parseCase opsP opsE) casesS
    c2 = map (\r -> TestCase (assertBool "case parsable" (isRight r)) ) cases
    (_,cases2) = partitionEithers cases
    c3 = TestCase (assertEqual "evaluatedRight"
                      (PMEval.eval w cases) expected)

tests = TestList
  [ test1 "test1" "C(1)" ["_ -> 42", "x->x"] (OK 42)
  ]

main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
   then
     exitWith ExitSuccess
   else
     exitWith (ExitFailure 1)
