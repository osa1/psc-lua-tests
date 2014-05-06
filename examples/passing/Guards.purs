module Main where

  import Prelude

  collatz = \x -> case x of
    y | y % 2 == 0 -> y / 2
    y -> y * 3 + 1

  -- Guards have access to current scope
  collatz2 = \x y -> case x of
    z | y > 0 -> z / 2
    z -> z * 3 + 1

  main = Debug.Trace.trace "Done"
