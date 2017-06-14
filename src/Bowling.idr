module Bowling

import Data.Vect


data Frame
  = Roll Int Int
  | Spare Int Int
  | Strike

BonusRolls : Frame -> Nat
BonusRolls Strike = 2
BonusRolls (Spare _ _) = 1
BonusRolls _ = 0


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
    remaining : Nat



  data GameCmd : GameState -> GameState -> Type where
    NewTurn : (frame : Frame) -> GameCmd (MkGameState (S (S n))) (MkGameState (S n))
    EndGame : (frame : Frame) -> (Vect (BonusRolls frame) Int) -> GameCmd (MkGameState (S Z)) (MkGameState 0)

  ||| Alias of the game
  record Game (turn : Nat) where
    constructor MkGame
    turns : Vect turn Frame

  ||| Create a new game
  new_game : (Version2.Game 0)
  new_game = ?hole








--
