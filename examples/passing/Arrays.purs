module Main where

import Prelude.Unsafe (unsafeIndex)
import Control.Monad.Eff

test1 arr = arr `unsafeIndex` 0 + arr `unsafeIndex` 1 + 1

test2 = \arr -> case arr of
  [x, y] -> x + y
  [x] -> x
  [] -> 0
  (x : y : _) -> x + y

data Tree = One Number | Some [Tree]

test3 = \tree sum -> case tree of
  One n -> n
  Some (n1 : n2 : rest) -> test3 n1 sum * 10 + test3 n2 sum * 5 + sum rest

test4 = \arr -> case arr of
  [] -> 0
  [_] -> 0
  x : y : xs -> x * y + test4 xs

main = do
  Debug.Trace.print $ test1 [1, 2, 3, 4]
  Debug.Trace.print $ test2 [1, 2]
  Debug.Trace.print $ test2 [1]
  Debug.Trace.print $ test2 []
  Debug.Trace.print $ test4 []
  Debug.Trace.trace "Done"
