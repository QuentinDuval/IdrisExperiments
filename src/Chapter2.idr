module Chapter2

palindrome : String -> Bool
palindrome s = s == reverse s

palindrome_nocase : String -> Bool
palindrome_nocase = palindrome . toLower

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : (Ord a) => List a -> List a
top_ten = take 10 . sortBy (flip compare)

over_length : Nat -> List String -> List String
over_length max_len = filter (\s => length s >= max_len)

repl_palindrome : IO ()
repl_palindrome = repl ">" ((++ "\n") . show . palindrome)
