module Data.Hashable
  ( HashCode(..)
  , hashCode
  , class Hashable
  , hash
  , hashEqual
  , combineHashes
  , hashDuplicate
  , Hour(..)
  ) where

import Prelude

import Data.Array (nubBy, length)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

-- Eq is a superclass of Hashable
class Eq a <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

derive instance eqHashCode :: Eq HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
-- Two values are 'hash-equal' if they are equal after each value has been passed thru the hash function
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

-- Map the hash function over the elements of the array (where element type is
-- also instance of Hashable) and then perform left fold over resulting hashes
-- using the combineHashes function.
instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

hashDuplicate :: forall a. Hashable a => Array a -> Boolean
hashDuplicate [] = false
hashDuplicate [x] = false
hashDuplicate xs = (length $ nubBy hashEqual xs) /= (length xs)

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour h) = HashCode (h `mod` 12)
