module Bowling

import Data.Vect


-- First version, trying to build a data structure with constructors
-- To constrain the type of a bowling game
namespace Version1

  data Frame
    = Roll Int Int
    | Spare Int Int
    | Strike

  data Game : (remaining : Nat) -> Type where
    NewGame : Game 10
    NewTurn : (frame : Frame) -> Game (S (S remaining)) -> Game (S remaining)
    LastTurn : (frame : Frame) -> (bonus : Vect 2 Int) -> Game (S Z) -> Game Z

  infixr 5 +>>

  (<+>) : Game (S (S n)) -> Frame -> Game (S n)
  (<+>) game frame = NewTurn frame game

  (+>>) : Game (S Z) -> (Frame, Vect 2 Int) -> Game Z
  (+>>) game (frame, bonus) = LastTurn frame bonus game

  game : Game 0
  game =
    NewGame
    <+> Roll 4 4
    <+> Roll 3 4
    <+> Roll 2 4
    <+> Roll 1 4
    <+> Roll 0 4
    <+> Roll 4 4
    <+> Roll 3 4
    <+> Roll 2 4
    <+> Roll 1 4
    +>> (Roll 4 4, [0, 0])
