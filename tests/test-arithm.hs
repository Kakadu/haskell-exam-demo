module Main where

import Arithm (eval, parse)
import Test.HUnit
import Data.Either
import System.Exit (exitFailure)

test1 name str expected =
  TestLabel name $ TestList [c1,c2]
  where
    rez = Arithm.parse str
    c1 = TestCase (assertBool "parsable" (isRight rez))
    (Right tree) = rez
    c2 = TestCase (assertEqual "parsable" (Arithm.eval tree) expected)

tests = TestList
  [ test1 "test1" "1+2*3" 7
  , test1 "test2" "5*3" 16
  ]
main = runTestTT tests

-- main = do
--     putStrLn "This test always fails!"
--     exitFailure
