module Main where

import Prelude

data Z
data S n

data Array n a = Array [a]

nil :: forall a. Array Z a
nil = Array []

foreign import concat
  "function concat(l1)\n\
  \  return function (l2)\n\
  \    local ret = {}\n\
  \    for _, v in ipairs(l1) do\n\
  \      table.insert(ret, v)\n\
  \    end\n\
  \    for _, v in ipairs(l2) do\n\
  \      table.insert(ret, v)\n\
  \    end\n\
  \    return ret\n\
  \  end\n\
  \end" :: forall a. [a] -> [a] -> [a]

cons' :: forall a n. a -> Array n a -> Array (S n) a
cons' x (Array xs) = Array $ concat [x] xs

foreign import error1
    "function error1(msg)\n\
    \  error(msg)\n\
    \end" :: forall a. String -> a

main = case cons' 1 $ cons' 2 $ cons' 3 nil of
         Array [1, 2, 3] -> Debug.Trace.trace "Done"
         _ -> error1 "Failed"
