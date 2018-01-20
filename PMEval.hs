module PMEval where

data EvalRez = OK Int | BadProgram | PMatchFail deriving (Show,Eq)

opsE = undefined
opsP = undefined

eval what cases = OK 42
