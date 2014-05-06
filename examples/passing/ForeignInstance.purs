module Main where

class Foo a where
  foo :: a -> String

foreign import instance fooArray :: (Foo a) => Foo [a]

foreign import instance fooNumber :: Foo Number

foreign import instance fooString :: Foo String

test1 _ = foo [1, 2, 3]

test2 _ = foo "Test"

main = Debug.Trace.trace "Done"
