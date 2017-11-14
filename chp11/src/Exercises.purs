module Exercises where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class
import Prelude

import Data.Array (replicate)
import Data.Foldable (traverse_)
import Data.String (joinWith, toCharArray)
import Data.Traversable (sequence)

testParens :: String -> Boolean
testParens s = (execState (unclosedCount $ toCharArray s) 0) == 0
  where
    unclosedCount :: Array Char -> State Int Unit
    unclosedCount = traverse_ \c -> modify \count -> count + (modifier c)

    modifier :: Char -> Int
    modifier '(' = 1
    modifier ')' = -1
    modifier _ = 0

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line s = do
  currentIndent <- ask
  pure $ (addIndent currentIndent <> s)

  where
    addIndent :: Int -> String
    addIndent n = joinWith "" (replicate n indentationString)

    indentationString :: String
    indentationString = " "

indent :: Doc -> Doc
indent = local (\n -> n + 1)

cat :: Array Doc -> Doc
cat xs = map (joinWith "\n") $ sequence xs

render :: Doc -> String
render d = runReader d 0
