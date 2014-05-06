module Main where

  import Prelude

  test :: forall a b. a -> b -> a
  test = \const _ -> const

  foreign import error1
    "function error1(msg)\n\
    \  error(msg)\n\
    \end" :: forall a. String -> a

  main = do
    let value = test "Done" {}
    if value == "Done"
      then Debug.Trace.trace "Done"
      else error1 "Not done"
