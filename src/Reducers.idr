module Reducers


--------------------------------------------------------------------------------
-- Core library
--------------------------------------------------------------------------------

StepL : (acc: Type) -> (elem: Type) -> Type
StepL acc elem = acc -> elem -> acc

ReducerL : (acc: Type) -> (a: Type) -> (b: Type) -> Type
ReducerL acc a b = StepL acc a -> StepL acc b

infixr 5 |>

namespace Reducer
  (|>) : ReducerL acc b c -> ReducerL acc a b -> ReducerL acc a c
  (|>) r2 r1 = r2 . r1

namespace Terminal
  (|>) : ReducerL acc inner outer -> StepL acc inner -> StepL acc outer
  (|>) r step = r step


--------------------------------------------------------------------------------
-- Useful reducers
--------------------------------------------------------------------------------

mapping : (outer -> inner) -> ReducerL acc inner outer
mapping fn step = \acc, outer => step acc (fn outer)

filtering : (elem -> Bool) -> ReducerL acc elem elem
filtering pf step = \acc, elem => if pf elem then step acc elem else acc


--------------------------------------------------------------------------------
-- Some tests
--------------------------------------------------------------------------------

run_test : IO ()
run_test = do
  printLn $ foldl (filtering (\e => e < 10) (mapping (* 2) (+))) 0 [1..100]
  printLn $ foldl ((filtering (\e => e < 10) |> mapping (*2)) (+)) 0 [1..100]
  printLn $ foldl (+) 0 (map (*2) (filter (\e => e < 10) [1..100]))
  printLn $ foldl (filtering (\e => e < 10) |> mapping (*2) |> (+)) 0 [1..100]
