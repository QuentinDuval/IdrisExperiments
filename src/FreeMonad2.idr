module FreeMonad2


data IOSpec : Type -> Type where
  Pure : a -> IOSpec a
  Bind : IOSpec a -> (a -> IOSpec b) -> IOSpec b
  WriteLine : String -> IOSpec ()
  ReadLine : IOSpec String

implementation Functor IOSpec where
  map fn expr = Bind expr (Pure . fn)

implementation Applicative IOSpec where
  pure = Pure
  fExpr <*> aExpr = Bind fExpr (\f => map f aExpr)

implementation Monad IOSpec where
  (>>=) = Bind

-- Factories

readLine : IOSpec String
readLine = ReadLine

prnLine : String -> IOSpec ()
prnLine s = WriteLine s

-- Interpreter

interpret : IOSpec r -> IO r
interpret (Pure a) = pure a
interpret (Bind a f) = interpret a >>= interpret . f
interpret ReadLine = getLine
interpret (WriteLine s) = putStrLn s

-- Guess

password : Nat -> (String -> Bool) -> IOSpec Bool
password maxAttempt valid = recur (S Z) where
  recur n = do
    prnLine "Password:"
    attempt <- readLine
    if valid attempt
      then do
        prnLine ("Successful login after " ++ show n ++ " attempt(s).")
        pure True
      else do
        prnLine ("Login failed: " ++ show n ++ " attempt(s).")
        if n < maxAttempt
          then recur (n + 1)
          else pure False

prog : IOSpec ()
prog = do
  granted <- password 3 (== "Kenobi")
  if granted then prnLine "Real program"
             else prnLine "Shutting down"

--

-- Combine a simulator

infixr 5 .|

data TestBotAction
  = Typing String
  | Thinking String

data TestBot = MkTestBot (List TestBotAction)

(.|) : TestBot -> IOSpec r -> IOSpec r
(.|) (MkTestBot actions) prog = pull actions prog where
  mutual

    pull : List TestBotAction -> IOSpec r -> IOSpec r
    pull p1 (Bind (WriteLine s) next)   = Bind (WriteLine s) (pull p1 . next)
    pull p1 (Bind ReadLine next)        = push p1 next
    pull p1 (Bind (Pure x) next)        = pull p1 (next x)
    pull p1 (Bind (Bind x next1) next2) = pull p1 (Bind x (\x => Bind (next1 x) next2))
    pull p1 x                           = x

    push : List TestBotAction -> (String -> IOSpec r)-> IOSpec r
    push [] cont                = Bind ReadLine cont
    push (Typing x::xs) cont    = pull xs (prnLine x *> cont x)
    push (Thinking x::xs) cont  = prnLine x *> push xs cont

--

fake : TestBot
fake = MkTestBot [Typing "Obiwan", Thinking "Search again...", Typing "Kenobi", Thinking "Ok I am done..."]

run_test : IO ()
run_test = interpret (fake .| prog)

--
