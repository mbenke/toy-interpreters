module Main where
import qualified Tiny1.Interpreter as Tiny1
import qualified Tiny2.Interpreter as Tiny2
import qualified Tiny3.Interpreter as Tiny3
import qualified Tiny3.Interpreter2 as Tiny3b
import qualified Tiny4.Interpreter as Tiny4

main :: IO()
main = do
  Tiny1.test
  Tiny2.test
  Tiny3.test
  Tiny3b.test
  Tiny4.test  

