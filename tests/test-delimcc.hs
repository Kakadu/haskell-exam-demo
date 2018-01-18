module Main where

import EvalDelimCC (eval, parse)
import System.IO.Unsafe
import Test.HUnit
import Data.Either
import System.Exit

test1 name str expected =
  TestLabel name $ TestList [c1,c2]
  where
    _  = unsafePerformIO (print rez)
    rez = parse str
    c1 = TestCase (assertBool "parsable" (isRight rez))
    (Right tree) = rez
    c2 = TestCase (assertEqual "parsable" (eval tree) expected)

tests = TestList
  [ test1 "test1" "1+2*3" (Just 42)
  , test1 "test2" "5*3"  (Just 42)
  ]

main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
   then
     exitWith ExitSuccess
   else
     exitWith (ExitFailure 1)
