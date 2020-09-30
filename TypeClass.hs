module TypeClass (
    Answer,
    YesNo
) where

import qualified Data.Map as Map;

data Answer a = Yes | No a deriving (Show, Ord);

data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show, Ord, Enum, Read, Eq, Bounded)

instance (Eq a) => Eq (Answer a) where
    Yes == Yes = True
    No a == No b = a == b
    _ == _ = False

data Tree a =
    Nil |
    Node a (Tree a) (Tree a)
    deriving (Eq, Show)


class YesNo a where
    yesno :: a -> Bool

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Float where
    yesno 0 = False
    yesno _ = True

instance YesNo Double where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno Nil = False
    yesno _ = True

type Name = String;
type PhoneNumber = String;
type PhoneBook = Map.Map Name PhoneNumber;

inPhoneBook :: (Name,PhoneNumber) -> PhoneBook -> Bool
inPhoneBook (name, number) pbook =
    case rec of
        Nothing -> False
        Just a -> a == number
    where rec = Map.lookup name pbook


