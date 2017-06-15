module Bowling

import Data.Vect

SpareBonus : Nat
SpareBonus = 1

StrikeBonus : Nat
StrikeBonus = 2

PinCount : Nat
PinCount = 10

FrameCount : Nat
FrameCount = 10

ValidFrame : (x, y: Nat) -> Type
ValidFrame x y = (x + y `LTE` PinCount, x `LT` PinCount, y `LT` PinCount)

data Frame : Type where
  TwoRolls : (x, y : Nat) -> { auto prf : ValidFrame x y } -> Frame
  Strike : Frame

roll : (x, y: Nat) -> { auto prf : ValidFrame x y } -> Frame
roll x y = TwoRolls x y

strike : Frame
strike = Strike

--

bonusRolls : Frame -> Nat
bonusRolls Strike = StrikeBonus
bonusRolls (TwoRolls x y) = if x + y == PinCount then SpareBonus else 0

record BowlingGame where
  constructor MkBowlingGame
  frames : Vect FrameCount Frame
  bonus : Vect (bonusRolls (last frames)) (Fin (S PinCount))

pins : Frame -> List Nat
pins (TwoRolls x y) = [x, y]
pins Strike = [PinCount]

nextRolls : Vect n Frame -> List Nat
nextRolls [] = []
nextRolls (f :: fs) = pins f ++ nextRolls fs

scores' : Nat -> Vect n Frame -> Vect bonus Nat -> Nat
scores' current [] bonus = current
scores' current (f :: fs) bonus =
  let diff = sum (pins f ++ take (bonusRolls f) (nextRolls fs ++ toList bonus))
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
  assertEq 300 $ score $ MkBowlingGame (replicate 10 strike) [10, 10]

should_be_164_for_spare_6_4_only_game : IO ()
should_be_164_for_spare_6_4_only_game =
  assertEq 164 $ score $ MkBowlingGame (replicate 10 (roll 6 4)) [10]

should_be_150_for_5_pins_only_game : IO ()
should_be_150_for_5_pins_only_game =
  assertEq 150 $ score $ MkBowlingGame (replicate 10 (roll 5 5)) [5]

should_be_90_for_best_game_without_bonus : IO ()
should_be_90_for_best_game_without_bonus =
  assertEq 90 $ score $ MkBowlingGame (replicate 10 (roll 6 3)) []

should_be_104_for_wikipedia_example : IO ()
should_be_104_for_wikipedia_example =
  assertEq 104 $ score $ MkBowlingGame ([strike, strike, strike, roll 8 2, roll 8 0] ++ replicate 5 (roll 0 0)) []

run_tests : IO ()
run_tests = do
  should_be_300_for_perfect_game
  should_be_164_for_spare_6_4_only_game
  should_be_150_for_5_pins_only_game
  should_be_90_for_best_game_without_bonus
  should_be_104_for_wikipedia_example

--
