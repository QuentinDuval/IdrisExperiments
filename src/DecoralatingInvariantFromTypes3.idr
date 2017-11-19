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

remainingQuantity : (e: Exercise) -> { auto prf : ValidExercise e } -> Double
remainingQuantity e = totalQty (option e) - exercisedQty e

test_option_1 : Double
test_option_1 =
  let e = MkExercise (MkOption "EUR/USD" 1000) 100
  in remainingQuantity e

conditionalQuantity : (e: Exercise) -> Maybe Double
conditionalQuantity e =
  case isItJust (validExercise e) of
    Yes _ => Just $ remainingQuantity e
    No _ => Nothing

test_option_2 : Maybe Double
test_option_2 =
  let e = MkExercise (MkOption "EUR/USD" 1000) 100
  in conditionalQuantity e

--
