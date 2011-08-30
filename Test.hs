module Main where
import qualified Tiny1.Interpreter as Tiny1
import qualified Tiny2.Interpreter as Tiny2

main :: IO()
main = do
  Tiny1.test
  Tiny2.test
