{-# LANGUAGE OverloadedStrings #-}

module Person where

import Data.String

data Person = Person String (Maybe Int)

instance IsString Person where
  fromString name = Person name Nothing

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = "Simon Peyton Jones"

showPerson :: Person -> String
showPerson (Person name (Just num)) = name ++ " and the number is : " ++ show num
showPerson (Person name Nothing) = name

mhTest2 :: Int -> String
mhTest2 = \case 3 -> "three"; 4 -> "four"; _ -> "wut?"