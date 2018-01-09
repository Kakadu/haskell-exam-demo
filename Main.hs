module Main where

import Prelude
import Control.Monad
import System.Exit
import System.Environment
import Text.JSON.Yocto

main :: IO ()
main = do
  args <- System.Environment.getArgs
  items <-
    case args of
      [file_name] -> readFile file_name
      _      -> putStrLn "File not specified" >> exitFailure
  programs <- case Text.JSON.Yocto.decode items of
    Array xs -> return xs
    _ -> print "JSON contents is not an array" >> exitFailure
  sequence $ map processJSON programs
  exitSuccess
  -- where
  --   vs :: IO Value
  --   vs =  return $


data Lam = Var String | Abs String Lam | App Lam Lam

processJSON :: Value -> IO ()
processJSON ast = do
  print $ ast
  return ()
  where 
    parse t = case t of
      Object m ->
      Array arr -> fold
