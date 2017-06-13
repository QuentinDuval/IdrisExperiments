module InOut

echo_double : IO ()
echo_double =
  repl ">" $ \input =>
    let n = the Int (cast input)
    in show (n * 2)

total
double_it : IO ()
double_it = do
  input <- getLine
  let n = the Int (cast input)
  printLn (n * 2)
