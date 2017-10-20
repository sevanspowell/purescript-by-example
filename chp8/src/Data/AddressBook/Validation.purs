module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..),
                         address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)
import Data.Maybe(Maybe(..))
import Data.Array(head, tail, sort, nub, foldM)
import Data.List

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _ _ = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _ _ = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _ _ _ = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value | test regex value = pure unit
matches field _ _ = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p

third :: forall a. Array a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  z  <- head zs
  pure z

sums :: Array Int -> Array Int
sums xs = sort $ nub $ foldM (\a b -> [a, b, a + b]) 0 xs

-- ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
-- apply :: forall a b. f (a -> b) -> f a -> f b
-- ap is equal to apply where apply 'f' is specialized to Monad

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  x' <- f x
  xs' <- filterM f xs
  pure (if x' then x : xs' else xs')

-- Prove lift2 f (pure a) (pure b) is equal to pure (f a b)
--
-- lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- lift2 f a b = f <$> a <*> b
--
-- So, lift2 f (pure a) (pure b) is equal to:
--   f <$> (pure a) <*> (pure b)
--
-- Given: <$> (map) f a = do
--          x <- a
--          pure (f x)
--
-- f <$> (pure a) <*> (pure b) is equal to:
--   pure (f a) <*> (pure b)
--   where types are: m (b -> c) <*> m b
--
-- Those types match our <*> function:
-- <*> (ap) is equal to:
--   ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
--   ap mf ma = do
--     f <- mf
--     a <- ma
--     pure (f a)
--
-- Therefore pure (f a) <*> (pure b) is equal to:
--   pure (f a b)
-- So, lift2 f (pure a) (pure b) is equal to pure (f a b)
