module Main where

  import Prelude

  type Foo a = [a]  

  foreign import length
    "function length(a)\n\
    \  return #a\n\
    \end" :: forall a. [a] -> Number

  foo _ = length ([] :: Foo Number)

  main = Debug.Trace.trace "Done"
