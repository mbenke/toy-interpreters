module Main where
import qualified Tiny1.Test as Tiny1
import qualified Tiny2.Test as Tiny2
import qualified Tiny3.Test as Tiny3
import qualified Tiny4.Interpreter as Tiny4

import qualified Simple1.Test as Simple1

main :: IO()
main = do
  Tiny1.test
  Tiny2.test
  Tiny3.test
  Tiny4.test  
  Simple1.test

