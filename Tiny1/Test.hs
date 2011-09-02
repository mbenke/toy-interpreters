module Tiny1.Test where
import Tiny1.Lexer
import Tiny1.Syntax
import Tiny1.Interpreter

prog1 :: Stmt
prog1 = SBlock [ 
  "x" := 1,
  "y" := 2,
  "c" := (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

text1 = "x=1y=2c=x+y"

testInterpreter :: IO()
testInterpreter = putStrLn $ runProg prog1

testLexer = print $ elex text1 