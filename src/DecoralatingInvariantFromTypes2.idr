module DecoralatingInvariantFromTypes2

import Data.SortedSet


-- Finding ways to validate this
-- * WAY NUMBER ONE: Invariant on each attributes
-- * WAT NUMBER TWO: Transform into validated data structures (template then???)

record Person where
  constructor MkPerson
  firstName : String
  lastName : String
  email : String

-- Attribute validity

data ValidName : (s : String) -> Type where
  MkValidName : ValidName s

upperCases : List Char
upperCases = ['A'..'Z']

lowerCases : List Char
lowerCases = ['a'..'z']

validName : (name : String) -> Maybe (ValidName name)
validName name = check (unpack name) where
  check [] = Nothing
  check (x::xs) =
    if elem x upperCases && all (\x => elem x lowerCases) xs
      then Just MkValidName
      else Nothing

data ValidMail : (s: String) -> Type where
  MkValidMail : ValidMail s

validMail : (email : String) -> Maybe (ValidMail email)
validMail email =
  if length (split (== '@') email) == 2
    then Just MkValidMail
    else Nothing

data ValidPerson : (p: Person) -> Type where
  MkValidPerson : ValidPerson p

validPerson : (p: Person) -> Maybe (ValidPerson p)
validPerson p =
  if isJust (validName (firstName p))
     && isJust (validName (lastName p))
     && isJust (validMail (email p))
    then Just MkValidPerson
    else Nothing

isValidPerson : (p: Person) -> Dec (IsJust (validPerson p))
isValidPerson p = isItJust (validPerson p)

-- How to check at inputs

doStuffOnValidGuy : (p: Person) -> { auto ok : IsJust (validPerson p) } -> String
doStuffOnValidGuy p = firstName p ++ lastName p ++ email p

run_test_1 : String
run_test_1 =
  let p = MkPerson "Obiwan" "Kenobi" "obiwan@kenobi.com"
  in doStuffOnValidGuy p

conditionalPersonStuff : Person -> String
conditionalPersonStuff p =
  let p' = record { firstName $= (++ "scotty") } p
  in case isValidPerson p' of
      Yes _ => doStuffOnValidGuy p'
      No _ => ""

run_test_2 : String
run_test_2 =
  let p1 = MkPerson "Obiwan" "Kenobi" "obiwan@kenobi.com"
      p2 = MkPerson "__!!" "??" "obiwan-kenobi.com"
  in conditionalPersonStuff p1 ++ conditionalPersonStuff p2


-- SECOND EXAMPLE
-- The same with records that have link between them
-- Allows to separate `updates` of record from validity

record Option where
  constructor MkOption
  instrument : String
  totalQty : Double

record Exercise where
  constructor MkExercise
  option : Option
  exercisedQty : Double

data ValidExercise : (e: Exercise) -> Type where
  MkValidExercise : ValidExercise e

validExercise : (e: Exercise) -> Maybe (ValidExercise e)
validExercise e =
  if exercisedQty e <= totalQty (option e) -- LT cannot work because of doubles...
    then Just MkValidExercise
    else Nothing

--

remainingQuantity : (e: Exercise) -> { auto prf : IsJust (validExercise e) } -> Double
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
