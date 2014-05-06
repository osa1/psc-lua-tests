module Main where

import Prelude

tail = \(_:xs) -> xs

foreign import error1
  "function error1(msg)\n\
  \  error(msg)\n\
  \end" :: forall a. String -> a

main = 
  let ts = tail [1, 2, 3] in
  if ts == [2, 3] 
  then Debug.Trace.trace "Done"
  else error1 "Incorrect result from 'tails'."
