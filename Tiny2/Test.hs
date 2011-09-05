module Tiny2.Test where
import Tiny2.Syntax
import qualified Tiny2.ParsecParser as Parser
import Tiny2.Interpreter

prog1 :: Stmt
prog1 = SBlock [ 
  "x" := 1,
  "y" := 2,
  "c" := (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

text1 = "x=1y=2 _=x+y"

testInterpreter :: IO()
testInterpreter = runProg prog1

testParser = case Parser.runParser "text1" text1 of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    runProg p

test = do
  testInterpreter
  testParser

