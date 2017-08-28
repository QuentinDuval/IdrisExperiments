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
describe = ?hole

logFmt : (schema: Schema) -> String -> SchemaType schema
logFmt [] out = out
logFmt (SInt :: rest) out = \i => logFmt rest (out ++ " " ++ show i)
logFmt (SString :: rest) out = \s => logFmt rest (out ++ " " ++ s)
logFmt (SLiteral s :: rest) out = logFmt rest (out ++ " " ++ s)

logEvent : (event: LogEvent) -> SchemaType (schema event)
logEvent e = logFmt (schema e) (code e ++ ":")

test_log : String
test_log =
  let s = [SInt, SLiteral "is the age of", SString]
  in logEvent (MkLogEvent "BIRTHDAY" s) 32 "Quentin"

--
