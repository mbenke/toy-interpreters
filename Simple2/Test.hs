module Simple2.Test where
import Simple2.Syntax
import Simple2.Interpreter

test :: IO ()
test = do
  putStrLn "Simple2/prog1"
  testInterpreter1
  putStrLn "  .../prog2"
  testInterpreter2

prog1 :: Stmt
prog1 = SBlock [ 
  SVar "x",
  SVar "y",
  "x" := 1,
  SBlock [ SVar "x", "x" := 42 ], -- local variable
  "y" := 2,
  SPrint (EVar "x"+ EVar "y")
  ]

prog2 = SExp $ ELets [
  ("l", ENew 1),
  ("x", EDeref (EVar "l")),
  ("_", (ELets [("x", 2)] ( 
    EIf (EVar "x") (EVar "x") 42))),
  ("y" , 2)
  ]
  (EVar "x"+ EVar "y")


-- undefined var
bad1 = SExp $ EVar "x"

testInterpreter1 = runProg prog1
testInterpreter2 = runProg prog2
