module DecoralatingInvariantFromTypes

import Data.Vect


record Matrix a where
  constructor MakeMatrix
  values : Vect width (Vect height a)

MatrixWidth : Matrix a -> Nat
MatrixWidth (MakeMatrix {width} v) = width

MatrixHeight : Matrix a -> Nat
MatrixHeight (MakeMatrix {height} v) = height

--

-- TODO: use nats... and LT

natToFin' : Nat -> (n : Nat) -> Maybe (Fin n)
natToFin' Z     (S j) = Just FZ
natToFin' (S k) (S j) with (natToFin' k j)
                          | Just k' = Just (FS k')
                          | Nothing = Nothing
natToFin' _ _ = Nothing

{-
castNatToFin : (x : Nat) -> (m : Nat) -> { auto ok : LT x m } -> Fin m
castNatToFin Z m' = the (Fin m') ?hole
castNatToFin (S x') (S m') {ok = LT (S x') (S m')} = FS (weaken (castNatToFin x' m'))
castNatToFin (S x') Z impossible
-}

total
castNatToFin : (x : Nat) -> (m : Nat) -> { auto ok : LT x m } -> Fin m
castNatToFin x (S m) = restrict m (cast x)

total
valueAt : (m : Matrix a) -> (x, y: Nat)
          -> { auto okX : LT x (MatrixWidth m) }
          -> { auto okY : LT y (MatrixHeight m) }
          -> a
valueAt m x y =
  let y' = castNatToFin y (MatrixHeight m)
      x' = castNatToFin x (MatrixWidth m)
  in Data.Vect.index y' (Data.Vect.index x' (values m))

--

diagSum : (Num a) => (m : Matrix a) -> (x, y: Nat)
          -> { auto okX : (LT x (MatrixHeight m), LT x (MatrixWidth m)) }
          -> { auto okY : (LT y (MatrixHeight m), LT y (MatrixWidth m)) }
          -> a
diagSum m x y {okX = (_,_)} {okY = (_,_)} =
  valueAt m x y
  + valueAt m y x

run_test : Integer
run_test =
  let m1 = MakeMatrix [[1, 2], [3, 4]]
  in valueAt m1 0 1 + valueAt m1 1 0


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
