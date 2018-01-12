module Main where

import Prelude hiding (abs)
import Data.String.Utils
import Control.Monad
import System.Exit
import System.Environment
import ULC
--import Text.JSON.Yocto

-- stripStr :: String -> IO String
-- stripStr = return . unpack . Data.Text.strip . pack

data Term =
    TmVar  String
  | TmAbs  String Term
  | TmApp  Term Term
  | TmReset  Term
  | TmCallCC  String Term
  deriving (Show)

ops :: Ops Term
ops = Record  { abs  = const TmAbs
              , var  = const TmVar
              , app  = const TmApp
              , reset = const TmReset
              , call  = const TmCallCC }

main :: IO ()
main = do
  args <- System.Environment.getArgs
  text <-
    case args of
      [file_name] -> rstrip <$> readFile file_name
      _      -> putStrLn "File not specified" >> exitFailure
  let lines = split "\n" text
  print lines
  print $ map (ULC.parse ops) lines
  -- programs <- case Text.JSON.Yocto.decode items of
  --   Array xs -> return xs
  --   _ -> print "JSON contents is not an array" >> exitFailure
  -- sequence $ map processJSON programs
  exitSuccess


-- data Lam = Var String | Abs String Lam | App Lam Lam
--
-- processJSON :: Value -> IO ()
-- processJSON ast = do
--   print $ ast
--   return ()
--   where
--     parse t = case t of
--       Object m ->
--       Array arr ->
