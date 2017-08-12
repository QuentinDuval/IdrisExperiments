module Money

Currency : Type
Currency = String

Amount : Type
Amount = Double

record Money where
  constructor MkMoney
  currency : Currency
  amount : Amount

Eq Money where
  lhs == rhs = currency lhs == currency rhs && amount lhs == amount rhs

data SameCurrency : (m1, m2: Money) -> Type where
  MkSameCurrency : (currency m1 = currency m2) -> SameCurrency m1 m2

addMoney : (lhs, rhs : Money) -> {auto prf : SameCurrency lhs rhs} -> Money
addMoney m1 m2 = MkMoney (currency m1) (amount m1 + amount m2)

sameCurrency : (lhs, rhs : Money) -> Dec (SameCurrency lhs rhs)
sameCurrency lhs rhs =
  case decEq (currency lhs) (currency rhs) of
    Yes prf => Yes (MkSameCurrency prf)
    No contra => No $ \(MkSameCurrency sameCurr) => contra sameCurr

test_money_1 : Bool
test_money_1 =
  -- Does not compile
  -- MkMoney "EUR" 3 == addMoney (MkMoney "EUR" 1) (MkMoney "USD" 2)
  MkMoney "EUR" 3 == addMoney (MkMoney "EUR" 1) (MkMoney "EUR" 2)

test_money_2 : Money -> Money -> Maybe Money
test_money_2 lhs rhs =
  -- Would not compile
  -- Just $ addMoney lhs rhs
  -- Compiles fine
  case sameCurrency lhs rhs of
    Yes _ => pure $ addMoney lhs rhs
    No _  => Nothing
