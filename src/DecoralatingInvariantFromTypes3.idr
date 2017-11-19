module DecoralatingInvariantFromTypes3

import DecoralatingInvariantFromTypes2

-- Client code

doStuffOnValidMail : (e: String) -> { auto ok : ValidMail e } -> String
doStuffOnValidMail e = e ++ " - validated"

doStuffOnValidGuy : (p: Person) -> { auto ok : ValidPerson p } -> String
doStuffOnValidGuy p {ok = MkValidPerson} = firstName p ++ lastName p ++ doStuffOnValidMail (email p)

run_test_1 : String
run_test_1 =
  let p = MkPerson "Obiwan" "Kenobi" "obiwan@kenobi.com" -- beware, depend on public exports...
  in doStuffOnValidGuy p

conditionalPersonStuff : Person -> String
conditionalPersonStuff p =
  let p' = record { firstName $= (++ "scotty") } p
  in case validPerson p' of
      Just _ => doStuffOnValidGuy p'
      Nothing => ""

run_test_2 : String
run_test_2 =
  let p1 = MkPerson "Obiwan" "Kenobi" "obiwan@kenobi.com"
      p2 = MkPerson "__!!" "??" "obiwan-kenobi.com"
  in conditionalPersonStuff p1 ++ conditionalPersonStuff p2

--
