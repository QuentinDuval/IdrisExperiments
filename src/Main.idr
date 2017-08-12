module Main

import Bowling
import BowlingUnsafe
import Chapter2
import Chapter3
import DoNotation
import Fibonacci
import HexagonalSNCF
import InOut
import Money
import Namespaces
import Records
import Reducers
import Strings

main : IO ()
main = do
  print (fibo_recur 1000)
  putStrLn ""
  print (fibo_iterate 1000)
  putStrLn ""
