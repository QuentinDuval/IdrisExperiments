module Logging


data SchemaElem = IntVar | StrVar | StrCst String

Schema : Type
Schema = List SchemaElem

SchemaType : Schema -> Type
SchemaType [] = String
SchemaType (IntVar :: xs) = Int -> SchemaType xs
SchemaType (StrVar :: xs) = String -> SchemaType xs
SchemaType (_ :: xs) = SchemaType xs

record Event where
  constructor MkEvent
  title : String
  messageSpec : Schema

Show SchemaElem where
  show IntVar = "{int}"
  show StrVar = "{string}"
  show (StrCst s) = s

Show Event where
  show e = title e ++ ": " ++ concatMap show (messageSpec e)

logEvent : (event: Event) -> SchemaType (messageSpec event)
logEvent e = loop (messageSpec e) (title e ++ ": ")
  where
    loop : (messageSpec: Schema) -> String -> SchemaType messageSpec
    loop [] out = out
    loop (IntVar :: rest) out = \i => loop rest (out ++ show i)
    loop (StrVar :: rest) out = \s => loop rest (out ++ s)
    loop (StrCst s :: rest) out = loop rest (out ++ s)

parseMessageSpec' : List Char -> Schema
parseMessageSpec' [] = []
parseMessageSpec' ('{' :: 'i' :: 'n' :: 't' :: '}' :: s) = IntVar :: parseMessageSpec' s
parseMessageSpec' ('{' :: 's' :: 't' :: 'r' :: 'i' :: 'n' :: 'g' :: '}' :: s) = StrVar :: parseMessageSpec' s
parseMessageSpec' (x :: s) =
  case parseMessageSpec' s of
    StrCst str :: rest => StrCst (strCons x str) :: rest
    rest => StrCst (strCons x "") :: rest

parseMessageSpec : String -> Schema
parseMessageSpec s = parseMessageSpec' (unpack s)

--
-- Approach without type safety for logEvent
--

data PlaceholderValue = IntVal Int | StrVal String

logEvent' : Event -> List PlaceholderValue -> Maybe String
logEvent' e vals = loop (messageSpec e) vals (title e ++ ": ")
  where
    loop : Schema -> List PlaceholderValue -> String -> Maybe String
    loop [] [] out = pure out
    loop (IntVar :: xs) (IntVal i :: ys) out = loop xs ys (out ++ show i)
    loop (StrVar :: xs) (StrVal s :: ys) out = loop xs ys (out ++ s)
    loop (StrCst x :: xs) ys out = loop xs ys (out ++ x)
    loop _ _ _ = Nothing


-- TODO: one with a vector of the right size! at compile time

--
-- Tests
--

birthDay : Event
birthDay = MkEvent "Happy BirthDay" [IntVar, StrCst " years old, ", StrVar]

newYearEve : Event
newYearEve = MkEvent "New Year's Eve" (parseMessageSpec "Happy new year {int}")
          -- MkEvent "New Year's Eve:" [StrCst "Happy new year ", IntVar]

allEvents : List Event
allEvents = [birthDay, newYearEve]

test_log : List String
test_log =
  map show allEvents
  ++
  [ logEvent birthDay 32 "Quentin"
  , logEvent newYearEve 2017
  ]
  ++
  mapMaybe id
  [ logEvent' birthDay [IntVal 32, StrVal "Quentin"]
  , logEvent' birthDay [StrVal "Quentin", IntVal 32]
  , logEvent' birthDay [StrVal "Quentin"]
  ]

--
