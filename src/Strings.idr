module Strings


caesar_cipher : Int -> String -> String
caesar_cipher shift input =
  let cipher = chr . (+ shift) . ord
  in pack $ map cipher (unpack input)
