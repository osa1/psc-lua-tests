module Main where

foreign import first "function first(xs) return xs[1] end" :: forall a. [a] -> a

foreign import loop "function loop() while true do end end" :: forall a. a

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

main = Debug.Trace.trace "Done"
