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

assertEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

should_be_300_for_perfect_game : IO ()
should_be_300_for_perfect_game =
  assertEq 300 $ score $ MkBowlingGame (replicate 10 Strike) [10, 10]

should_be_164_for_spare_6_4_only_game : IO ()
should_be_164_for_spare_6_4_only_game =
  assertEq 164 $ score $ MkBowlingGame (replicate 10 (Roll 6 4)) [10]

should_be_90_for_best_game_without_bonus : IO ()
should_be_90_for_best_game_without_bonus =
  assertEq 90 $ score $ MkBowlingGame (replicate 10 (Roll 6 3)) []

should_be_104_for_wikipedia_example : IO ()
should_be_104_for_wikipedia_example =
  assertEq 104 $ score $ MkBowlingGame ([Strike, Strike, Strike, Roll 8 2, Roll 8 0] ++ replicate 5 (Roll 0 0)) []

run_tests : IO ()
run_tests = do
  should_be_300_for_perfect_game
  should_be_164_for_spare_6_4_only_game
  should_be_90_for_best_game_without_bonus
  should_be_104_for_wikipedia_example

--
