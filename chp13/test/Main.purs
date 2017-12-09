module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (intersect, length, sortBy)
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (quickCheck, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

ints :: Array Int -> Array Int
ints = id

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id

treeOfInt :: Tree Number -> Tree Number
treeOfInt = id

bools :: Array Boolean -> Array Boolean
bools = id

arrayLength :: forall a. Array a -> Int
arrayLength = len 0 <<< fromFoldable
  where len count (Cons x1 t@(Cons _ _)) = len (count + 1) t
        len count (Cons _ Nil) = len (count + 1) Nil
        len count Nil = count

main :: Eff ( console :: CONSOLE
            , random :: RANDOM
            , exception :: EXCEPTION
            ) Unit
main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys ->
    let
      result = merge (sorted xs) (sorted ys)
    in
      isSorted result <?> "Merging " <> show xs <> " and " <> show ys <> " did not result in a sorted array: " <> show result
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys -> isSorted $ bools $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys

  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a $ treeOfInt t
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t

  quickCheck $ \xs -> xs == (merge xs [])
    <?> "Merging " <> show xs <> " with an empty array doesn't result in: " <> show xs <> "."

  quickCheck $ \xs ->
      let
        l1 = (length (ints xs))
        l2 = (arrayLength (ints xs))
      in
        l1 == l2 <?> "Array " <> show xs <> " returned length " <> show l1 <> ", should of returned length " <> show l2
