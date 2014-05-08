module Main where

  import Prelude ()
  import Control.Monad.Eff
  import Debug.Trace

  fns = \f -> if f true then f else \x -> x

  not = \x -> if x then false else true

  id = \x -> x

  main = do
    if not (fns id true)
      then trace "false"
      else trace "true"
