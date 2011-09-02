module Tiny1.Test where
import Tiny1.Lexer
import Tiny1.Syntax
import Tiny1.ParsecParser
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

testLexer :: IO()
testLexer = print $ elex text1 

testParser :: IO()
testParser = case runParser "text1" . elex $ text1 of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    putStrLn $ runProg p

test = do
  testInterpreter
  testLexer
  testParser

