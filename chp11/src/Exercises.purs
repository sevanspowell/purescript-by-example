module Exercises where

import Data.Int
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Array
import Data.Foldable (traverse_)
import Data.String (joinWith, toCharArray, drop, take)
import Data.Traversable (sequence)
import Data.Monoid.Additive
import Data.Tuple
import Data.Either
import Prelude
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Except.Trans

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

-- sumArray :: Array Number -> State Number Unit
-- sumArray = traverse_ \n -> modify \sum -> sum + n

sumArray :: Array Number -> Writer (Additive Number) Unit
sumArray = traverse_ $ \n -> tell (Additive n)
-- Tell appends provided value to current accumulated result
-- Additive monoid pluses n on append:
--   Additive x <> Additive y == Additive (x + y)

collatzIterationsLog :: Int -> Tuple Int (Array String)
collatzIterationsLog = runWriter <<< f 0
  where
  f :: Int -> Int -> Writer (Array String) Int
  f x 1 = do
    _ <- log 1
    pure x
  f x n = do
    _ <- log n
    f (x + 1) (collatz n)

  log :: forall t. MonadTell (Array String) t => Int -> t Unit
  log n = tell ["collatzLog " <> show n]

collatzIterations :: Int -> Int
collatzIterations = f 0
  where
  f :: Int -> Int -> Int
  f x 1 = x
  f x n = f (x + 1) (collatz n)

collatz :: Int -> Int
collatz 1 = 1
collatz n = if even n
              then (n / 2)
              else ((3 * n) + 1)

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty string"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

writerAndExceptT :: ExceptT String (Writer (Array String)) String
writerAndExceptT = do
  _ <- lift $ tell ["Before the error"]
  _ <- throwError "Error!"
  _ <- lift $ tell ["After the error"]
  pure "Return Value"
