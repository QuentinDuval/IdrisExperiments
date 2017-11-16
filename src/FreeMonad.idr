module FreeMonad


data IOSpec a
  = ReadLine (String -> IOSpec a)
  | WriteLine String (IOSpec a)
  | Pure a

{-
data IOSpec : Type -> Type where
  ReadLine : (String -> IOSpec a) -> IOSpec a
  WriteLine : String (IOSpec a) -> IOSpec a
  Pure : a -> IOSpec a
-}

implementation Functor IOSpec where
  map f = assert_total recur where
    recur (Pure a) = Pure (f a)
    recur (ReadLine cont) = ReadLine (recur . cont)
    recur (WriteLine s next) = WriteLine s (recur next)

implementation Applicative IOSpec where
  pure = Pure
  (<*>) pf a = assert_total (recur pf) where
    recur (Pure f) = map f a
    recur (ReadLine cont) = ReadLine (recur . cont)
    recur (WriteLine s next) = WriteLine s (recur next)

implementation Monad IOSpec where
  (>>=) ma f = assert_total (recur ma) where
    recur (Pure a) = f a
    recur (ReadLine cont) = ReadLine (recur . cont)
    recur (WriteLine s next) = WriteLine s (recur next)

--

readLine : IOSpec String
readLine = ReadLine (\s => Pure s)

prnLine : String -> IOSpec ()
prnLine s = WriteLine s (Pure ())

interpret : IOSpec r -> IO r
interpret (Pure a) = pure a
interpret (ReadLine cont) = do
  l <- getLine
  interpret (cont l)
interpret (WriteLine s next) = do
  putStrLn s
  interpret next

--

password' : Nat -> (String -> Bool) -> IO Bool
password' maxAttempt valid = recur (S Z) where
  recur n = do
    putStrLn "Password:"
    attempt <- getLine
    if valid attempt
      then do
        putStrLn ("Successful login after " ++ show n ++ " attempt(s).")
        pure True
      else do
        putStrLn ("Login failed: " ++ show n ++ " attempt(s).")
        if n < maxAttempt
          then recur (n + 1)
          else pure False

-- Abstracted

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

-- Combine a simulator

infixr 5 .|

data TestBotAction
  = Typing String
  | Thinking String

data TestBot = MkTestBot (List TestBotAction)

(.|) : TestBot -> IOSpec r -> IOSpec r
(.|) (MkTestBot actions) prog = pull actions prog where
  mutual -- To define mutually recursive functions

    pull : List TestBotAction -> IOSpec r -> IOSpec r
    pull actions (Pure r) = Pure r
    pull actions (WriteLine s next) = do
      prnLine s          -- Print the line to display (such as "Password:")
      pull actions next  -- Keep on traversing the IOSpec computation
    pull actions (ReadLine cont) =
      push actions cont -- Give control to bot actions

    push : List TestBotAction -> (String -> IOSpec r )-> IOSpec r
    push [] cont = ReadLine cont
    push (Typing x::xs) cont = do
      prnLine x         -- Display the value typed by the bot
      pull xs (cont x)  -- Send `x` to the main program, and give it control back
    push (Thinking x::xs) cont = do
      prnLine x         -- Print the thoughts of our bot
      readLine          -- Wait for a line (notification to continue)
      push xs cont      -- Keep going

-- Tests

fake : TestBot
fake = MkTestBot [ Typing "Spock"
                 , Thinking "Search again..."
                 , Typing "Scotty"
                 , Thinking "Ok I am done..."]

run_test : IO Bool
run_test = interpret (fake .| password 3 (== "Scotty"))

--
