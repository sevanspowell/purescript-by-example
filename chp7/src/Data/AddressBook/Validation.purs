module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence, sequenceDefault, traverse, class Traversable)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)
import Data.Foldable
import Data.Monoid

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (notWhitespace "Street" o.street *> pure o.street)
          <*> (notWhitespace "City"   o.city   *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (notWhitespace "First Name" o.firstName *> pure o.firstName)
         <*> (notWhitespace "Last Name"  o.lastName  *> pure o.lastName)
         <*> traverse validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[a-zA-Z]{2}$" noFlags of
      Right r -> r

notWhitespaceRegex :: Regex
notWhitespaceRegex =
  unsafePartial
    case regex "^.*[^\\s]+.*$" noFlags of
      Right r -> r

notWhitespace :: String -> String -> V Errors Unit
notWhitespace fieldName str = matches fieldName notWhitespaceRegex str

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l x r) = Branch (map f l) (f x) (map f r)

instance foldableTree :: Foldable Tree where
  foldr f acc (Branch l x r) = foldr f (f x (foldr f acc r)) l
  foldr f acc Leaf = acc
  foldl f acc (Branch l x r) = foldl f (f (foldl f acc l) x) r
  foldl f acc Leaf = acc
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r
  foldMap f Leaf = mempty

instance inOrderTraverseTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch l x r) = Branch `map` traverse f l <*> f x <*> traverse f r
  sequence = traverse id

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch l x r) = "(Branch " <> show l <> " " <> show x <> " " <> show r <> ")"

traverse' :: forall a b m t. Applicative m => Traversable t => (a -> m b) -> t a -> m (t b)
traverse' f xs = sequence (map f xs)

sequence' :: forall a m t. Applicative m => Traversable t => t (m a) -> m (t a)
sequence' = traverse id
