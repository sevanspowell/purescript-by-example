module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- findEntry is the composition of a filtering function and the head function
findEntry :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
findEntry predicate = head <<< filter predicate

findEntryByFirstAndLastName :: String -> String -> AddressBook -> Maybe Entry
findEntryByFirstAndLastName firstName lastName = findEntry predicate
  where
    predicate :: Entry -> Boolean
    predicate entry = entry.firstName == firstName && entry.lastName == lastName


findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = findEntry predicate 
  where
    predicate :: Entry -> Boolean
    predicate entry = entry.address.street == address.street
                        && entry.address.city == address.city
                        && entry.address.state == address.state

doesFirstOrLastNameAppear :: String -> AddressBook -> Boolean
doesFirstOrLastNameAppear name = not <<< null <<< filter containsName
  where
    containsName :: Entry -> Boolean
    containsName entry = entry.firstName == name
                           || entry.lastName == name

isDuplicateAddress :: Address -> Address -> Boolean
isDuplicateAddress a1 a2 = a1.street == a2.street
                             && a1.city == a2.city
                             && a1.state == a2.state

isDuplicateEntry :: Entry -> Entry -> Boolean
isDuplicateEntry e1 e2 = e1.firstName == e2.firstName
                           && e1.lastName == e2.lastName
                           && isDuplicateAddress e1.address e2.address


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy isDuplicateEntry
