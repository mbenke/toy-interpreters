module Simple3.Test where
import Simple3.Syntax
import Simple3.Interpreter
import qualified Simple3.ParsecParser as Parser

derefVar :: Name -> Exp
derefVar = EDeref . EVar

test :: IO ()
test = do
--  putStrLn "Simple3/prog1"
 -- testInterpreter1
  putStrLn "  .../prog2"
  runProg prog2
  testParser "text2" text2
  putStrLn "  .../prog3"
  runProg prog3

testParser name text = case Parser.runParser name text of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    runProg p

prog2 :: Defs
prog2 = [
  ("l", ENew 1),
  ("x", EDeref (EVar "l")),
  ("_", (ELets [("x", 2)] ( 
    EIf (EVar "x") (EVar "x") 42))),
  ("y" , 2),
  ("_",EVar "x"+ EVar "y")
  ]
prog3 = [ 
  ("y", 
   (ELet "x" (ENew 42) (EVar "x"))),
  ("_",derefVar "y") ]
-- undefined var
bad1 = [("_", EVar "x")]

text2 = "l=new 1 x=*l \
\ _ = let x = 2 in if x then x else 42\
\ y = 2\
\ _ = x+y"