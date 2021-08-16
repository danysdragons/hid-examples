module Helpers2 where

import Data.Function
import Data.List

-- Predicate indicates elements to *exclude*, contrary to how the standard filter functino works
filterOut :: (a -> Bool) -> [a] -> [a]
filterOut pred = filter (pred `fscomp` not)

infixl 1 `pipe`

pipe :: a -> (a -> b) -> b
a `pipe` b = a & b

testPipe :: String
testPipe = ((2 *) `fscomp` show) <$> [1 .. 50] `pipe` intercalate "<:>"

infixl 9 `fscomp`

fscomp :: (a -> b) -> (b -> c) -> a -> c
a `fscomp` b = b . a

infixl 9 .>>

(.>>) :: (a -> b) -> (b -> c) -> a -> c
a .>> b = b . a

tripleNewComp = (* 3) .>> show