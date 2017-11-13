module Exercises where

import Prelude

import Data.String (toCharArray)
import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

testParens :: String -> Boolean
testParens s = (execState (unclosedCount $ toCharArray s) 0) == 0
  where
    unclosedCount :: Array Char -> State Int Unit
    unclosedCount = traverse_ \c -> modify \count -> count + (modifier c) 

    modifier :: Char -> Int
    modifier '(' = 1
    modifier ')' = -1
    modifier _ = 0
