module Logging

-- infixr 5 .+.

data SchemaElem = SInt | SString | SLiteral String

Schema : Type
Schema = List SchemaElem

SchemaType : Schema -> Type
SchemaType [] = String
SchemaType (SInt :: rest) = Int -> SchemaType rest
SchemaType (SString :: rest) = String -> SchemaType rest
SchemaType (_ :: rest) = SchemaType rest

record Event where
  constructor MkEvent
  title : String
  schema : Schema

describe : Event -> String
describe e = loop (schema e) (title e ++ ": ")
  where
    loop : (schema: Schema) -> String -> String
    loop [] out = out
    loop (SInt :: xs) out = loop xs (out ++ "{int}")
    loop (SString :: xs) out = loop xs (out ++ "{string}")
    loop (SLiteral s :: xs) out = loop xs (out ++ s)

logEvent : (event: Event) -> SchemaType (schema event)
logEvent e = loop (schema e) (title e ++ ": ")
  where
    loop : (schema: Schema) -> String -> SchemaType schema
    loop [] out = out
    loop (SInt :: rest) out = \i => loop rest (out ++ show i)
    loop (SString :: rest) out = \s => loop rest (out ++ s)
    loop (SLiteral s :: rest) out = loop rest (out ++ s)

parseSchema' : List Char -> Schema
parseSchema' [] = []
parseSchema' ('{' :: 'i' :: 'n' :: 't' :: '}' :: s) = SInt :: parseSchema' s
parseSchema' ('{' :: 's' :: 't' :: 'r' :: 'i' :: 'n' :: 'g' :: '}' :: s) = SString :: parseSchema' s

{-
parseSchema' s =
  let (a, b) = span (/= '{') s
  in SLiteral (pack a) :: parseSchema' b
-}

-- TODO: this works, with the break, it fails, maybe due to Delay (:printdef)
parseSchema' (x :: s) =
  case parseSchema' s of
    SLiteral str :: rest => SLiteral (strCons x str) :: rest
    rest => SLiteral (strCons x "") :: rest

parseSchema : String -> Schema
parseSchema s = parseSchema' (unpack s)

--
-- Tests
--

birthDay : Event
birthDay = MkEvent "Happy BirthDay" [SInt, SLiteral " years old, ", SString]

newYearEve : Event
newYearEve =
  MkEvent "New Year's Eve:" (parseSchema "Happy new year {int}")
  -- MkEvent "New Year's Eve:" [SLiteral "Happy new year ", SInt]

allEvents : List Event
allEvents = [birthDay, newYearEve]

test_log : List String
test_log =
  map describe allEvents
  ++
  [ logEvent birthDay 32 "Quentin"
  , logEvent newYearEve 2017
  ]

--
