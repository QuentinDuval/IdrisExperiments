module Namespaces

infixr 5 +.

namespace Scalar
  (+.) : Int -> Int -> Int
  x +. y = x + y

namespace Vector
  (+.) : List Int -> List Int -> List Int
  xs +. ys = zipWith (+) xs ys

test_overload : IO ()
test_overload = do
  printLn $ 3 +. 5
  printLn $ [1, 2, 3] +. [4, 5]
