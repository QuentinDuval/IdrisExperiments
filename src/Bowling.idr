module Bowling

import Data.Vect


data Frame
  = Roll Int Int
  | Spare Int Int
  | Strike

pins : Frame -> List Int
pins (Roll x y) = [x, y]
pins (Spare x y) = [x, y]
pins Strike = [10]

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


-- Thrid version, using commands to separate the transitions from repr
namespace Version3

  record BowlingState where
    constructor MkBowlingState
    frames : Vect 10 Frame
    bonus : Vect (BonusRolls (last frames)) Int

  ||| A sample game
  game3 : BowlingState
  game3 = MkBowlingState
            [Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4,
             Roll 4 4, Roll 4 4, Roll 4 4, Roll 4 4, Strike]
            [10, 8]

  throws' : Vect n Frame -> List Int
  throws' [] = []
  throws' (f :: fs) = pins f ++ throws' fs

  scores' : Vect n Frame -> Vect bonus Int -> Int
  scores' [] bonus = 0
  scores' (Roll x y :: fs) bonus = x + y + scores' fs bonus
  scores' (Spare x y :: fs) bonus
    = x + y + sum (take 1 (throws' fs ++ toList bonus)) + scores' fs bonus
  scores' (Strike :: fs) bonus
    = 10 + sum (take 2 (throws' fs ++ toList bonus)) + scores' fs bonus

  score : BowlingState -> Int
  score (MkBowlingState frames bonus) = scores' frames bonus

  test_v3 : IO ()
  test_v3 = printLn (score game3)







--
