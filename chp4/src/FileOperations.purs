module FileOperations where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven $ x - 2 * (x / 2)

numEven :: Array Int -> Int
numEven arr =
  if null arr
    then 0
    else if isEven (unsafePartial head arr)
           then 1 + numEven (unsafePartial tail arr)
           else numEven (unsafePartial tail arr)
