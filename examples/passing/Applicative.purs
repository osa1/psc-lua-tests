module Main where

import Prelude (id, Show, (++), ($))
import Control.Monad.Eff

class Applicative f where
  pure :: forall a. a -> f a
  (<*>) :: forall a b. f (a -> b) -> f a -> f b

data Maybe a = Nothing | Just a

instance applicativeMaybe :: Applicative Maybe where
  pure = Just
  (<*>) (Just f) (Just a) = Just (f a)
  (<*>) _ _ = Nothing


showMaybe (Just s) = "Just " ++ s
showMaybe Nothing = "Nothing"

main = do
  Debug.Trace.trace (showMaybe (pure id <*> pure "done"))
  Debug.Trace.trace (showMaybe (pure id <*> Nothing))
  Debug.Trace.trace (showMaybe (Nothing <*> pure "done"))
