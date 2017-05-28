module Chapter3

import Data.Vect

||| Return the length of each of the string of the list
all_lengths : List String -> List Nat
all_lengths [] = []
all_lengths (x :: xs) = length x :: all_lengths xs

mutual
  is_even : Nat -> Bool
  is_even Z = True
  is_even (S k) = is_odd k

  is_odd : Nat -> Bool
  is_odd Z = False
  is_odd (S k) = is_even k

vec_lengths : Vect l String -> Vect l Nat
vec_lengths [] = []
vec_lengths (x :: xs) = length x :: vec_lengths xs
