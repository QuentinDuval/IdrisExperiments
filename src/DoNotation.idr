module DoNotation

data StringBuilder
  = Pure String
  | Bind StringBuilder (String -> StringBuilder)

(>>=) : StringBuilder -> (String -> StringBuilder) -> StringBuilder
(>>=) = Bind

eval_string : StringBuilder -> String
eval_string (Pure s) = s
eval_string (Bind x f) = eval_string (f (eval_string x))

build_john : StringBuilder
build_john = do
  john <- Pure "John"
  let john_doe = john ++ " Doe" ++ concat (replicate 10 "!")
  Pure john_doe
