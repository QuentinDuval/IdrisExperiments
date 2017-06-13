module Main

import Chapter2
import Chapter3
import DoNotation
import Fibonacci
import InOut
import Namespaces
import Records
import Strings

main : IO ()
main = do
  print (fibo_recur 1000)
  putStrLn ""
  print (fibo_iterate 1000)
  putStrLn ""
