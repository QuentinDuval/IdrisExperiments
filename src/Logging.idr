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
  schema : Schema

Show Event where
  show e = title e ++ ": " ++ concatMap describeElem (schema e)
    where
      describeElem IntVar = "{int}"
      describeElem StrVar = "{string}"
      describeElem (StrCst s) = s

logEvent : (event: Event) -> SchemaType (schema event)
logEvent e = loop (schema e) (title e ++ ": ")
  where
    loop : (schema: Schema) -> String -> SchemaType schema
    loop [] out = out
    loop (IntVar :: rest) out = \i => loop rest (out ++ show i)
    loop (StrVar :: rest) out = \s => loop rest (out ++ s)
    loop (StrCst s :: rest) out = loop rest (out ++ s)

parseSchema' : List Char -> Schema
parseSchema' [] = []
parseSchema' ('{' :: 'i' :: 'n' :: 't' :: '}' :: s) = IntVar :: parseSchema' s
parseSchema' ('{' :: 's' :: 't' :: 'r' :: 'i' :: 'n' :: 'g' :: '}' :: s) = StrVar :: parseSchema' s
parseSchema' (x :: s) =
  case parseSchema' s of
    StrCst str :: rest => StrCst (strCons x str) :: rest
    rest => StrCst (strCons x "") :: rest

parseSchema : String -> Schema
parseSchema s = parseSchema' (unpack s)

--
-- Approach without type safety for logEvent
--

data SchemaValue = IntVal Int | StrVal String

logEvent' : Event -> List SchemaValue -> Maybe String
logEvent' e vals = loop (schema e) vals (title e ++ ": ")
  where
    loop : Schema -> List SchemaValue -> String -> Maybe String
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
newYearEve = MkEvent "New Year's Eve" (parseSchema "Happy new year {int}")
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
