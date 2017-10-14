module Main where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (tail)
import Data.Array.Partial (head) as Partial
import Partial.Unsafe (unsafePartial)
import Data.Foldable
import Data.Monoid

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

showComplex :: Complex -> String
showComplex (Complex {real, imaginary }) =
  "[Complex: real: " <> show real <> ", imaginary: " <> show imaginary <> "]"

instance showComplexNumber :: Show Complex where
  show complexNumber = showComplex complexNumber

eqComplex :: Complex -> Complex -> Boolean
eqComplex (Complex {real: r1, imaginary: im1}) (Complex {real: r2, imaginary: im2}) =
  (r1 == r2 && im1 == im2)

instance eqComplexNumber :: Eq Complex where
  eq a b = eqComplex a b

data NonEmpty a = NonEmpty a (Array a)

nonEmpty :: forall a. Array a -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty xs = Just (NonEmpty (unsafePartial Partial.head xs) (fromMaybe [] $ tail xs))

showNonEmpty :: forall a. Show a => Show (Array a) => NonEmpty a -> String
showNonEmpty (NonEmpty x xs) = "[NonEmpty " <> show x <> " " <> show xs <> "]"

instance showNonEmptyInstance :: (Show a, Show (Array a)) => Show (NonEmpty a) where
  show n = showNonEmpty n

eqNonEmpty :: forall a. Eq a => Eq (Array a) => NonEmpty a -> NonEmpty a -> Boolean
eqNonEmpty (NonEmpty x xs) (NonEmpty y ys) = (x == y && xs == ys)

instance eqNonEmptyInstance :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq a b = eqNonEmpty a b

appendNonEmpty :: forall a. Semigroup (Array a) => NonEmpty a -> NonEmpty a -> NonEmpty a
appendNonEmpty (NonEmpty x xs) (NonEmpty y ys) = NonEmpty (x) (xs <> [y] <> ys)

instance semigroupNonEmptyInstance :: Semigroup (Array a) => Semigroup (NonEmpty a) where
  append a b = appendNonEmpty a b

nonEmptyMap :: forall a b. Functor (Array) => (a -> b) -> NonEmpty a -> NonEmpty b
nonEmptyMap f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance functorNonEmpty :: Functor (Array) => Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable (Array) => Foldable NonEmpty where
  foldMap :: forall a m. Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap a (NonEmpty x xs) = foldMap a xs

  foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b
  foldl f acc (NonEmpty x xs) = foldl f (f acc x) xs

  foldr :: forall a b. (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (NonEmpty x xs) = f x (foldr f acc xs)

data Extended a = Finite a | Infinite

instance eqExtendedInstance :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = (x == y)
  eq _ _ = false

instance ordExtendedInstance :: (Eq a, Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare (Finite _ ) Infinite = LT 
  compare Infinite (Finite _) = GT
  compare (Finite x) (Finite y) = compare x y

