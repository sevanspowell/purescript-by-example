module Exercises where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Class
import Data.Array
import Data.Either
import Data.Int
import Data.Monoid.Additive
import Data.Tuple
import Prelude

import Control.Comonad (extract)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, joinWith, stripPrefix, take, toCharArray)
import Data.String as Data.String
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

-- type Printer = ReaderT Level (WriterT (Array String) Identity)

-- line' :: String -> Printer String
-- line' s = do
--   currentIndent <- ask 
--   tell $ [(addIndent currentIndent <> s)]

--   where
--     addIndent :: Int -> String
--     addIndent n = joinWith "" (replicate n indentationString)

--     indentationString :: String
--     indentationString = " "

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

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split' :: Parser String
split' = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

-- extract, from Pursuit:
-- Comonad extends the Extend class with the extract function which extracts a
-- value, discarding the comonadic context.
-- Comonad is the dual of Monad, and extract is the dual of pure.
runParser p s = extract $ runExceptT $ runWriterT $ runStateT p s

safeDivide :: Number -> Number -> ExceptT Errors Identity Number
safeDivide a 0.0 = throwError ["Divide by zero"]
safeDivide a b = pure (a / b)

string :: String -> Parser String
string "" = lift $ lift $ throwError ["Empty string provided"]
string xs = do
  s <- get
  lift $ tell ["The state is " <> show s]

  case (stripPrefix (Pattern xs) s) of
    Nothing -> lift $ lift $ throwError ["Not a prefix"]
    (Just leftOver) -> do
      put (drop prefixLength s)
      pure (xs) 

  where
    prefixLength :: Int
    prefixLength = Data.String.length $ xs
