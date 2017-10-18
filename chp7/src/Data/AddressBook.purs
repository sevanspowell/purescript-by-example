module Data.AddressBook where

import Prelude
import Control.Apply
import Data.List
import Data.Maybe

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number = PhoneNumber
  { "type": ty
  , number: number
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " <>
    "{ type: "   <> show o."type" <>
    ", number: " <> show o.number <>
    " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " <>
    "{ firstName: "   <> show o.firstName <>
    ", lastName: "    <> show o.lastName <>
    ", homeAddress: " <> show o.homeAddress <>
    ", phones: "      <> show o.phones <>
    " }"

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

liftAddition :: forall a f. Apply f => Semiring a => f a -> f a -> f a
liftAddition x y = lift2 (+) x y

liftSubtraction :: forall a f. Apply f => Ring a => f a -> f a -> f a
liftSubtraction x y = lift2 (-) x y

liftMultiplication :: forall a f. Apply f => Semiring a => f a -> f a -> f a
liftMultiplication x y = lift2 (*) x y

liftDivision :: forall a f. Apply f => EuclideanRing a => f a -> f a -> f a
liftDivision x y = lift2 (/) x y

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x
