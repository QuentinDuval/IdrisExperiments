module DecoralatingInvariantFromTypes

import Data.Vect


record Matrix a where
  constructor MakeMatrix
  values : Vect width (Vect height a)

MatrixWidth : Matrix a -> Nat
MatrixWidth (MakeMatrix {width} _) = width

MatrixHeight : Matrix a -> Nat
MatrixHeight (MakeMatrix {height} _) = height

data LTWidth : (n : Nat) -> (m : Matrix a) -> Type where
  MkLTWidth : {auto ok : LT n (MatrixWidth m)} -> LTWidth n m

data LTHeight : (n : Nat) -> (m : Matrix a) -> Type where
  MkLTHeight : {auto ok : LT n (MatrixHeight m)} -> LTHeight n m

data LTWidthHeight : (n : Nat) -> (m : Matrix a) -> Type where
  MkLTWidthHeight :
    { auto okX : LTWidth n m } -> { auto okY : LTHeight n m } -> LTWidthHeight n m

isLTWidth : (x : Nat) -> (m : Matrix a) -> Dec (LTWidth x m)
isLTWidth n m =
  case isLTE (S n) (MatrixWidth m) of
    No contra => No $ \(MkLTWidth {ok}) => contra ok
    Yes prf => Yes (MkLTWidth {ok = prf})

isLTHeight : (y : Nat) -> (m : Matrix a) -> Dec (LTHeight y m)
isLTHeight n m =
  case isLTE (S n) (MatrixHeight m) of
    No contra => No $ \(MkLTHeight {ok}) => contra ok
    Yes prf => Yes (MkLTHeight {ok = prf})

--

total
castNatToFin : (x : Nat) -> (m : Nat) -> { auto ok : LT x m } -> Fin m
castNatToFin x Z impossible
castNatToFin x (S m) = restrict m (cast x)

total
valueAt : (m : Matrix a) -> (x, y: Nat)
          -> { auto okX : LTWidth x m }
          -> { auto okY : LTHeight y m }
          -> a
valueAt m x y {okX = MkLTWidth} {okY = MkLTHeight} =
  let y' = castNatToFin y (MatrixHeight m)
      x' = castNatToFin x (MatrixWidth m)
  in Data.Vect.index y' (Data.Vect.index x' (values m))

--

diagSum : (Num a) => (m : Matrix a) -> (x, y: Nat)
          -> { auto okX : LTWidthHeight x m }
          -> { auto okY : LTWidthHeight y m }
          -> a
diagSum m x y {okX = MkLTWidthHeight} {okY = MkLTWidthHeight} =
  valueAt m x y + valueAt m y x

run_test : Integer
run_test =
  let m1 = MakeMatrix [[1, 2, 3], [4, 5, 6]]
  in diagSum m1 0 1
     + valueAt m1 1 2
     + diagSum m1 1 1

run_test' : (Num a) => Matrix a -> Nat -> Nat -> a
run_test' m x y =
  case isLTWidth x m of
    No _ => 0
    Yes okX =>
      case isLTHeight y m of
        No _ => 0
        Yes okY => valueAt m x y


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
