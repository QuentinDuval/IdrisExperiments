module DecoralatingInvariantFromTypes

import Data.Vect


record Matrix a where
  constructor MakeMatrix
  values : Vect width (Vect height a)

{-
record Matrix a where
  constructor MakeMatrix
  values : List (List a)

InMatrixBounds : (m : Matrix a) -> (x, y: Nat) -> { ok: InBounds x (values m) } -> Type
InMatrixBounds m x y =
  ( InBounds x (values m)
  , InBounds y (index x (values m)) )

inMatrixBounds : (m : Matrix a) -> (x, y: Nat) -> Dec (InMatrixBounds m x y)
inMatrixBounds m x y =
  case inBounds x (values m) of
    No contraX => No (\(p1, p2) => contraX p1)
    Yes prfX => case inBounds y (index x (values m)) of
      No contraY => No (\(p1, p2) => contraY p2)
      Yes prfY => Yes (prfX, prfY)

valueAt : (m : Matrix a) -> (x, y : Nat) -> { auto ok : InMatrixBounds m x y } -> a
valueAt m x y {ok = (xOk, yOk)} = index y (index x (values m))

run_test : Integer
run_test =
  -- valueAt (the (Matrix Integer) $ MakeMatrix [[1, 2], [3, 4]]) Z Z
  let m1 = MakeMatrix [[1, 2], [3, 4]]
  in case inMatrixBounds m1 Z Z of
          Yes prf => valueAt m1 Z Z {ok = prf}
          No _ => the Integer 0
-}

--
