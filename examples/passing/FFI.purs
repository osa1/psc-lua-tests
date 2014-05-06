module Main where

foreign import foo
  "function foo(s)\n\
  \  return s\n\
  \end" :: String -> String

bar :: String -> String
bar _ = foo "test"

main = Debug.Trace.trace "Done"
