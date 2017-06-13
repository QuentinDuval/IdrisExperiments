module Fibonacci

export
fibo_recur : Int -> Integer
fibo_recur n = loop 0 1 n
  where
    loop curr next 0 = curr
    loop curr next n = loop next (curr + next) (n - 1)

export
fibo_iterate : Nat -> Integer
fibo_iterate n = fst (index n (iterate next_fib (0, 1)))
  where
    next_fib (curr, next) = (next, curr + next)
