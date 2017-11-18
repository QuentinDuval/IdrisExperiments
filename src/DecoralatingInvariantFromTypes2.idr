module DecoralatingInvariantFromTypes2


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

validName : (name : String) -> Maybe (ValidName name)
validName name =
  if name == "titi"
    then Just MkValidName
    else Nothing

data ValidMail : (s: String) -> Type where
  MkValidMail : ValidMail s

validMail : (email : String) -> Maybe (ValidMail email)
validMail email =
  if email == "titi@gmail.com"
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

-- How to check at inputs

{-
isValidName : (name : String) -> Dec (ValidName name)
isValidName name =
  case isItJust (validName name) of
    Yes ItIsJust => ?k
    No _ => ?hole
-}

onValidGuy : (p: Person) -> { auto prf : IsJust (validPerson p) } -> String
onValidGuy p = ?hole

--
