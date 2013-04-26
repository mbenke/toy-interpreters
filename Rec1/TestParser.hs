module Rec1.TestParser where
import Rec1.Parser

test1 = parse pExp "t1" "x"
test2 =  runParser "text2" text2
text2 = "l=new; l.x = 1 ;\
\ let x = l.x in if x then x else 42;\
\ y = 2;\
\ l.x"
