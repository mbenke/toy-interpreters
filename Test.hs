module Main where
import qualified Tiny1.Interpreter as Tiny1
import qualified Tiny2.Interpreter as Tiny2
import qualified Tiny3.Interpreter as Tiny3

main :: IO()
main = do
  Tiny1.test
  Tiny2.test
  Tiny3.test
  