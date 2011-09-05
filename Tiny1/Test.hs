module Tiny1.Test where
import Tiny1.Lexer
import Tiny1.Syntax
import qualified Tiny1.ParsecParser as Parser1
import qualified Tiny1.ParsecParser2 as Parser2
import Tiny1.Interpreter

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
testInterpreter = putStrLn $ runProg prog1

testLexer :: IO()
testLexer = print $ elex text1 

testParser1 :: IO()
testParser1 = case Parser1.runParser "text1" . elex $ text1 of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    putStrLn $ runProg p

testParser2 = case Parser2.runParser "text1" text1 of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    putStrLn $ runProg p

test = do
  testInterpreter
  testLexer
  testParser2

