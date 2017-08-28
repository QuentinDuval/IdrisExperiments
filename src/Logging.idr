module Logging

-- infixr 5 .+.

data Schema
  = SInt Schema
  | SString Schema
  | SLiteral String Schema
  | SEnd

SchemaType : Schema -> Type
SchemaType SEnd = String
SchemaType (SInt rest) = Int -> SchemaType rest
SchemaType (SString rest) = String -> SchemaType rest
SchemaType (SLiteral _ rest) = SchemaType rest

record LogEvent where
  constructor MkLogEvent
  code : String
  schema : Schema

describe : LogEvent -> String
describe = ?hole

logFmt : (schema: Schema) -> String -> SchemaType schema
logFmt (SInt rest) out = \i => logFmt rest (out ++ " " ++ show i)
logFmt (SString rest) out = \s => logFmt rest (out ++ " " ++ s)
logFmt (SLiteral s rest) out = logFmt rest (out ++ " " ++ s)
logFmt SEnd out = out

logEvent : (event: LogEvent) -> SchemaType (schema event)
logEvent e = logFmt (schema e) (code e ++ ":")

test_log : String
test_log =
  let s = SInt (SString SEnd)
  in logEvent (MkLogEvent "YOH" s) 1 "hello"

--
