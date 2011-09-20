module Simple5.Test where
import Simple5.Syntax
import Simple5.Interpreter
import qualified Simple5.ParsecParser as Parser

import Control.Monad.Error

derefVar :: Name -> Exp
derefVar = EDeref . EVar

test :: IO ()
test = do
  testException
  putStrLn "  .../prog2"
  runProg prog2
  {-
  testParser "text2" text2
  putStrLn "  .../prog3"
  runProg prog3
  putStrLn "  .../text4, expect None"
  testParser "text4" text4
  -}
  putStrLn ".../prog5, expect 2"
  runProg prog5

  putStrLn ".../prog6, expect Break \"foo\""
  runProg prog6 
  putStrLn ".../prog7, expect 1"
  -- runProg prog7 
  testParser "text7" text7
  
testIM :: IM () -> IO ()
testIM m = do
  res <- runIM m initState
  case res of
    Left e -> putWordsLn ["Error:",e]
    Right (a,state) -> do
      case a of
        Left exc -> putStrLn ("Exception: "++show exc)
        Right ok -> putStrLn ("OK: "++show ok)
      printState state      

putWordsLn :: [String] -> IO ()
putWordsLn = putStrLn . unwords

testException :: IO ()
testException = do
  putStrLn "Expect ()"
  testIM $ return ()
  putStrLn "Expect Error: Expected error"
  testIM $ throwError "Expected error"
  putStrLn "Expect ()"
  testIM $
    (throwError "this error should be caught") `catchError` (\s->return ())
    
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

text2 = "l=new 1; x=*l \
\ let x = 2 in if x then x else 42\
\ y = 2\
\ x+y"

text4 = "locals = new {}; \
\ locals.y=1; \
\ if locals.y then locals.y = 42 else locals.y = None;\ 
\ locals.y"

-- expect 2
prog5 = [("_", ELabel "return" (ELet "x" ( 1) 2))]
-- expect exception "foo"
prog6 = [("_", ELabel "return" (ELet "x" (EBreak"foo" 1) 2))]
-- expect 1
prog7 = [("_", ELabel "return" (ELet "x" (EBreak"return" 1) 2))]
text7 = "return: let x=break return 1 in 2"