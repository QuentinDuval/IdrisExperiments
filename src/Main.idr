module Main

import Chapter2
import Chapter3
import Fibonacci
import Namespaces

main : IO ()
main = do
  print (fibo_recur 1000)
  putStrLn ""
  print (fibo_iterate 1000)
  putStrLn ""
