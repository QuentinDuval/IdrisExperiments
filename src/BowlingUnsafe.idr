module BowlingUnsafe

import Data.Vect

data Frame : Type where
  Roll : (x : Fin 10) -> (y : Fin 10) -> Frame
  Strike : Frame

bonusRolls : Frame -> Nat
bonusRolls Strike = 2
bonusRolls (Roll x y) = if finToNat x + finToNat y == 10 then 1 else 0

record BowlingGame where
  constructor MkBowlingGame
  frames : Vect 10 Frame
  bonus : Vect (bonusRolls (last frames)) (Fin 11)

pins : Frame -> List Nat
pins (Roll x y) = [finToNat x, finToNat y]
pins Strike = [10]

throws : Vect n Frame -> List Nat
throws [] = []
throws (f :: fs) = pins f ++ throws fs

scores' : Nat -> Vect n Frame -> Vect bonus Nat -> Nat
scores' current [] bonus = current
scores' current (f :: fs) bonus =
  let diff = sum (pins f ++ take (bonusRolls f) (throws fs ++ toList bonus))
  in scores' (current + diff) fs bonus

score : BowlingGame -> Nat
score (MkBowlingGame frames bonus) = scores' 0 frames (map finToNat bonus)

-- Tests

run_tests : IO ()
run_tests = do
  printLn $ score $ MkBowlingGame (replicate 9 (Roll 4 4) ++ [Strike]) [10, 8]
  printLn $ score $ MkBowlingGame (replicate 10 Strike) [10, 10]
  printLn $ score $ MkBowlingGame (replicate 10 (Roll 6 4)) [10]
  printLn $ score $ MkBowlingGame ([Strike, Strike, Strike, Roll 8 2, Roll 8 0] ++ replicate 5 (Roll 0 0)) []

--
