module Main where

import Prelude

test 100 = 100
test n = test(1 + n)

foreign import error1
  "function error1(msg)\n\
  \  error(msg)\n\
  \end" :: forall a. String -> a

main = case test 0 of
         100 -> Debug.Trace.trace "Done"
         _ -> error1 "Failed
