module Main

import Chapter2

main : IO ()
main = do
  print (fibo_recur 1000)
  putStrLn ""
  print (fibo_iterate 1000)
  putStrLn ""
