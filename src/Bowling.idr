module Bowling

import Data.Vect

data IsValidFrame : Fin 10 -> Fin 10 -> Type where
  ValidFrame : LTE (finToNat x + finToNat y) 10 -> IsValidFrame x y

-- TODO: Change the encoding to have Fin 11 and Fin 10 for Spare
-- FAIL: (but then we need also Fin 10 < Fin 11, does not solve the issue)

{-
x : Fin 10
y : Fin 10
contra : LTE (plus (finToNat x) (finToNat y)) 10 -> Void
--------------------------------------
invalid_frame : IsValidFrame x y -> Void

And we have
ValidFrame : LTE (finToNat x + finToNat y) 10 -> IsValidFrame x y

How to build a contradiction of the type?
invalid_frame : IsValidFrame x y -> Void

It looks like continuation...
invalid_frame : (LTE (plus (finToNat x) (finToNat y)) 10 -> Void) -> IsValidFrame x y -> Void

Or look like we need a function "IsValidFrame x y -> LTE (x + y) 10" (ALLOW COMPOSITION)

-}

invalid_frame : (contra : LTE (plus (finToNat x) (finToNat y)) 10 -> Void) -> IsValidFrame x y -> Void
invalid_frame contra (ValidFrame prf) = contra prf

is_valid_frame : (x : Fin 10) -> (y : Fin 10) -> Dec (IsValidFrame x y)
is_valid_frame x y with (isLTE (finToNat x + finToNat y) 10)
  is_valid_frame x y | (Yes prf) = Yes (ValidFrame prf)
  is_valid_frame x y | (No contra) = No (invalid_frame contra)

data Frame : Type where
  Roll : (x : Nat) -> (y : Nat) -> Frame
  Spare : (x : Nat) -> (y : Nat) -> Frame
  Strike : Frame

pins : Frame -> List Nat
pins (Roll x y) = [x, y]
pins (Spare x y) = [x, y]
pins Strike = [10]

BonusRolls : Frame -> Nat
BonusRolls Strike = 2
BonusRolls (Spare _ _) = 1
BonusRolls _ = 0

{-

-- First version, trying to build a data structure with constructors
-- To constrain the type of a bowling game
namespace Version1

  data Game : (remaining : Nat) -> Type where
    NewGame : Game 10
    NewTurn : (frame : Frame) -> Game (S (S remaining)) -> Game (S remaining)
    LastTurn : (frame : Frame) -> (bonus : Vect (BonusRolls frame) Int) -> Game (S Z) -> Game Z

  infixr 5 +>>

  (<+>) : Game (S (S n)) -> Frame -> Game (S n)
  (<+>) game frame = NewTurn frame game

  (+>>) : Game (S Z) -> (frame : Frame) -> Vect (BonusRolls frame) Int -> Game Z
  (+>>) game frame bonus = LastTurn frame bonus game

  game1 : Game 0
  game1 =
    LastTurn (Roll 4 4) [] (NewGame
                            <+> Roll 4 4
                            <+> Roll 3 4
                            <+> Roll 2 4
                            <+> Roll 1 4
                            <+> Roll 0 4
                            <+> Roll 4 4
                            <+> Roll 3 4
                            <+> Roll 2 4
                            <+> Roll 1 4)




-- Second version, using commands to separate the transitions from repr
namespace Version2

  record GameState where
    constructor MkGameState
    turn_nb : Nat

  data GameCmd : GameState -> GameState -> Type where
    NewTurn : (frame : Frame) -> GameCmd (MkGameState n) (MkGameState (S n))
    EndGame : (frame : Frame) -> (Vect (BonusRolls frame) Int) -> GameCmd (MkGameState 9) (MkGameState 10)

  ||| Create a new game
  new_game : Vect 0 Frame
  new_game = []

  ||| Add a turn
  (<+>) : Vect n Frame
          -> GameCmd (MkGameState n) (MkGameState (S n))
          -> Vect (S n) Frame
  (<+>) xs (NewTurn frame) = frame :: xs
  (<+>) xs (EndGame frame ys) = frame :: xs

  ||| A sample game
  game2 : Vect 10 Frame
  game2 = new_game
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> NewTurn (Roll 4 4)
          <+> EndGame Strike [10, 10]

-}


-- Thrid version, using commands to separate the transitions from repr
namespace Version3

  record BowlingState where
    constructor MkBowlingState
    frames : Vect 10 Frame
    bonus : Vect (BonusRolls (last frames)) (Fin 11)

  ||| A sample game
  game3 : BowlingState
  game3 = MkBowlingState
            [Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4,
             Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4, Strike]
            [10, 8]

  throws' : Vect n Frame -> List Nat
  throws' [] = []
  throws' (f :: fs) = pins f ++ throws' fs

  scores' : Vect n Frame -> Vect bonus Nat -> Nat
  scores' [] bonus = 0
  scores' (Roll x y :: fs) bonus = x + y + scores' fs bonus
  scores' (Spare x y :: fs) bonus
    = x + y + sum (take 1 (throws' fs ++ toList bonus)) + scores' fs bonus
  scores' (Strike :: fs) bonus
    = 10 + sum (take 2 (throws' fs ++ toList bonus)) + scores' fs bonus

  score : BowlingState -> Nat
  score (MkBowlingState frames bonus) = scores' frames (map finToNat bonus)

  test_v3 : IO ()
  test_v3 = do
    printLn $ score $ MkBowlingState (replicate 9 (Roll 4 4) ++ [Strike]) [10, 8]
    printLn $ score $ MkBowlingState (replicate 10 Strike) [10, 10]
    printLn $ score $ MkBowlingState (replicate 10 (Spare 6 4)) [10]
    printLn $ score $ MkBowlingState ([Strike, Strike, Strike, Spare 8 2, Roll 8 0] ++ replicate 5 (Roll 0 0)) []







--
