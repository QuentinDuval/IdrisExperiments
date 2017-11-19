module DecoralatingInvariantFromTypes2

import Data.SortedSet


-- Finding ways to validate this
-- * WAY NUMBER ONE: Invariant on each attributes
-- * WAT NUMBER TWO: Transform into validated data structures (template then???)

public export
record Person where
  constructor MkPerson
  firstName : String
  lastName : String
  email : String

-- Attribute validity

public export
upperCases : List Char
upperCases = ['A'..'Z']

public export
lowerCases : List Char
lowerCases = ['a'..'z']

public export
validName : (name : String) -> Maybe String
validName name = check (unpack name) where
  check [] = Nothing
  check (x::xs) =
    if elem x upperCases && all (\x => elem x lowerCases) xs
      then Just name
      else Nothing

public export
data ValidName : (s : String) -> Type where
  MkValidName : {auto ok : IsJust (validName s)} -> ValidName s

public export
validMail : (email : String) -> Maybe String
validMail email =
  if length (split (== '@') email) == 2
    then Just email
    else Nothing

public export
data ValidMail : (s: String) -> Type where
  MkValidMail : {auto ok : IsJust (validMail s)} -> ValidMail s

public export
data ValidPerson : (p: Person) -> Type where
  MkValidPerson :
    { auto okFirstName : ValidName (firstName p) }
    -> { auto okLastName : ValidName (lastName p) }
    -> { auto okEmail : ValidMail (email p) }
    -> ValidPerson p

public export
validPerson : (p: Person) -> Maybe (ValidPerson p)
validPerson p =
  case isItJust (validName (firstName p)) of
    No _ => Nothing
    Yes _ => case isItJust (validName (lastName p)) of
      No _ => Nothing
      Yes _ => case isItJust (validMail (email p)) of
        No _ => Nothing
        Yes _ => Just MkValidPerson

--------------------------------------------------------------------------------
-- Check DecoralatingInvariantFromTypes3
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- SECOND EXAMPLE
-- The same with records that have link between them
-- Allows to separate `updates` of record from validity
--------------------------------------------------------------------------------

public export
record Option where
  constructor MkOption
  instrument : String
  totalQty : Double

public export
record Exercise where
  constructor MkExercise
  option : Option
  exercisedQty : Double

public export
validExercise : (e: Exercise) -> Maybe Exercise
validExercise e =
  if exercisedQty e <= totalQty (option e) -- LT cannot work because of doubles...
    then Just e
    else Nothing

public export
data ValidExercise : (e: Exercise) -> Type where
  MkValidExercise : {auto ok : IsJust (validExercise e)} -> ValidExercise e

--
