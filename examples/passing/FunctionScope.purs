module Main where

  import Prelude

  mkValue :: Number -> Number
  mkValue id = id

  foreign import error1
    "function error1(msg)\n\
    \  error(msg)\n\
    \end" :: forall a. String -> a

  main = do
    let value = mkValue 1
    if value == 1
      then Debug.Trace.trace "Done"
      else error1 "Not done"
