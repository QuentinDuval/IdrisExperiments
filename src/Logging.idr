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

parseSchema' : List Char -> Maybe Schema
parseSchema' [] = pure []
parseSchema' ('{' :: s) = do
  let (xs, ys) = break (== '}') s
  case nonEmpty ys of
    No _ => Nothing
    Yes _ => do
      schema <- parseSchema' (tail ys)
      if xs == unpack "int"
        then pure $ SInt :: schema
        else if xs == unpack "string"
          then pure $ SString :: schema
          else Nothing
parseSchema' s = do
  let (a, b) = break (== '{') s
  schema <- parseSchema' b
  pure $ SLiteral (pack a) :: schema

parseSchema : String -> Maybe Schema
parseSchema s = parseSchema' (unpack s)

--
-- Tests
--

birthDay : Event
birthDay = MkEvent "Happy BirthDay" [SInt, SLiteral " years old, ", SString]

newYearEve : Event
newYearEve =
  -- let Just schema = parseSchema "Happy new year {int}"
  -- in MkLogEvent "New Year's Eve:" schema
  MkEvent "New Year's Eve:" [SLiteral "Happy new year ", SInt]

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
