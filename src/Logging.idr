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

record LogEvent where
  constructor MkLogEvent
  code : String
  schema : Schema

describe : LogEvent -> String
describe e = loop (schema e) (code e ++ ": ")
  where
    loop : (schema: Schema) -> String -> String
    loop [] out = out
    loop (SInt :: xs) out = loop xs (out ++ "{int}")
    loop (SString :: xs) out = loop xs (out ++ "{string}")
    loop (SLiteral s :: xs) out = loop xs (out ++ s)

logEvent : (event: LogEvent) -> SchemaType (schema event)
logEvent e = logFmt (schema e) (code e ++ ": ")
  where
    logFmt : (schema: Schema) -> String -> SchemaType schema
    logFmt [] out = out
    logFmt (SInt :: rest) out = \i => logFmt rest (out ++ show i)
    logFmt (SString :: rest) out = \s => logFmt rest (out ++ s)
    logFmt (SLiteral s :: rest) out = logFmt rest (out ++ s)

parseSchema : List Char -> Maybe Schema
parseSchema [] = pure []
parseSchema ('{' :: s) = do
  let (xs, ys) = break (== '}') s
  case nonEmpty ys of
    No _ => Nothing
    Yes _ => do
      schema <- parseSchema (tail ys)
      if xs == unpack "int"
        then pure $ SInt :: schema
      else if xs == unpack "string"
        then pure $ SString :: schema
      else Nothing
parseSchema s = do
  let (a, b) = break (== '{') s
  schema <- parseSchema b
  pure $ SLiteral (pack a) :: schema


birthDay : LogEvent
birthDay = MkLogEvent "Happy BirthDay" [SInt, SLiteral " years old, ", SString]

newYearEve : LogEvent
newYearEve = MkLogEvent "New Year's Eve:" [SLiteral "Happy new year ", SInt]

--
-- Tests
--

test_log : List String
test_log =
  map describe [birthDay, newYearEve]
  ++
  [ logEvent birthDay 32 "Quentin"
  , logEvent newYearEve 2017 ]

--
