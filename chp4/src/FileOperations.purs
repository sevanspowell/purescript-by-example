module FileOperations where

import Data.Foldable
import Data.Path
import Prelude

import Control.MonadZero (guard)
import Data.Array (null, filter, (..), length, (:), concatMap)
import Data.Array.Partial (head, tail)
import Data.Maybe (Maybe(..))
import Data.Semigroup (append)
import Math (pow)
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

squares :: Array Number -> Array Number
squares = map (\x -> x * x)

removeNegatives :: Array Number -> Array Number
removeNegatives = filter (\x -> x >= 0.0)

infixl 4 filter as <$?>

removeNegativesInfix :: Array Number -> Array Number
removeNegativesInfix arr = (\x -> x >= 0.0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  [[i, j]]

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n -- Terminates branch of array comprehension if false
  [[i, j]]

isPrime :: Int -> Boolean
isPrime x = (length $ factors x) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a1 a2 = do
  i <- a1
  j <- a2
  [[i, j]]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. (n - 1)
  b <- 1 .. (n - 1)
  c <- 1 .. (n - 1)
  guard $ a*a + b*b == c*c
  [[a, b, c]]

factorizations :: Int -> Array (Array Int)
factorizations 1 = [[1, 1]]
factorizations n = (append $ factorizations $ n - 1) $ do
  i <- 1 .. n
  j <- 1 .. n
  guard $ i * j == n
  [[i, j]]

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (\f -> not $ isDirectory f) <<< allFiles

largestFile :: Path -> Path
largestFile = foldl (\xs x -> if size(x) > size(xs) then x else xs) root <<< onlyFiles

smallestFile :: Path -> Path
smallestFile = foldl (\xs x -> if (isDirectory xs || size(x) < size(xs)) then x else xs) root <<< onlyFiles

whereIs :: String -> Maybe Path
whereIs name = foldr (\x xs -> Just x) Nothing $ do
  f <- onlyFiles root
  guard $ filename f == name
  [f]
