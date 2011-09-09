module Simple3.Test where
import Simple3.Syntax
import Simple3.Interpreter

derefVar :: Name -> Exp
derefVar = EDeref . EVar

test :: IO ()
test = do
--  putStrLn "Simple3/prog1"
 -- testInterpreter1
  putStrLn "  .../prog2"
  runProg prog2
  putStrLn "  .../prog3"
  runProg prog3

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

