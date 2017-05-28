module Chapter3

||| Return the length of each of the string of the list
all_lengths : List String -> List Nat
all_lengths [] = []
all_lengths (x :: xs) = length x :: all_lengths xs
