module M1 (bar) where

  foo :: Number  
  foo = 0

  bar :: Number
  bar = foo

module Main where

  import M1

  testBar = bar

  main = Debug.Trace.trace "Done"
