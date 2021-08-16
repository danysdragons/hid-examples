module Helpers
  ( pipe,
    filterOut,
    compLR,
    test77,
    test78,
    test79,
    test80,
    test81,
    test82,
    test82alt,
    test83a,
    test83b,
    test84a,
    test84b,
    test84c,
    test84d,
    doubleString,
    fscomp,
    pipe_old,
    (.>>),
    tripleShow,
  )
where

import Data.Function
import Data.List

-- Use the F# pipe operator as a synonym for &
-- Probably not the best idea
(|>) :: a -> (a -> b) -> b
(|>) = (&)

a `pipe2` b = a & b

pipe_old = (&)

testPipe = filterOut even [1 .. 50] `pipe_old` fmap (* 2)

-- Compose reversed, analagous to >> in F# (>> has different meaning in Haskell)
(.>) :: (a -> b) -> (b -> c) -> a -> c
a .> b = b . a

lrcomp :: (a -> b) -> (b -> c) -> a -> c
a `lrcomp` b = b . a

comprev :: (a -> b) -> (b -> c) -> a -> c
comprev = lrcomp

compLR :: (a -> b) -> (b -> c) -> a -> c
compLR = lrcomp

-- Predicate indicates elements to *exclude*, contrary to how the standard filter functino works
filterOut :: (a -> Bool) -> [a] -> [a]
filterOut pred = filter (pred .> not)

test3a :: [String]
test3a = (show . (* 2)) <$> [1 .. 50]

-- test3b :: String
-- test3b = (show . (*2)) <$> [1 .. 50] `pipe` intercalate ":"

-- test4a :: String
-- test4a = ((2*) `compLR` show) <$> [1 .. 50] `pipe` intercalate ":"

-- test4b :: String
-- test4b = ((2*) `compLR` show) <$> [1 .. 50] `pipe` intercalate ":"

-- test5a :: String
-- test5a = ((*2) `compLR` show) <$> [1 .. 50] `pipe` intercalate ":"

test77 :: String
test77 = fmap show [1 .. 50] & intercalate ":"

test78 :: String
test78 = fmap (show . (* 2)) [1 .. 50] & intercalate ":"

test79 :: String
test79 = fmap ((* 2) `compLR` show) [1 .. 50] `pipe_old` intercalate ":"

test80 :: [String]
test80 = ((2 *) `compLR` show) <$> [1 .. 50]

------

infixl 1 `pipe`

pipe :: a -> (a -> b) -> b
a `pipe` b = a & b

test81 :: String
test81 = (((2 *) `compLR` show) <$> [1 .. 50]) `pipe_old` intercalate ":"

test82 :: String
test82 = ((2 *) `compLR` show) <$> [1 .. 50] & intercalate ":"

test82alt :: String
test82alt = ((2 *) `compLR` show) <$> [1 .. 50] `pipe` intercalate "<:>"

-- in test82, `pipe_old` cannot be substituted for &
-- test81 works because of the extra parens
-- test82alt shows use of `pipe`, which was given an explicit operator definition, including precedence etc, instead of just pipe = (&)
-----

test83b :: ([String], String)
test83b = fmap (negate `compLR` show) [1 .. 57] `pipe` (\strs -> (strs, "intercalated: " ++ intercalate "--" strs))

test83a :: ([String], String)
test83a = (negate `compLR` show) <$> [1 .. 57] `pipe` (\x -> (x, "intercalated: " ++ intercalate "--" x))

--test84a :: ([String], String)
test84a = [1 .. 57] `pipe` fmap (negate `compLR` show)

test84b = [1 .. 57] `pipe` fmap (negate `fscomp` show)

test84c = [1 .. 57] `pipe` fmap (negate `fscomp` show `fscomp` doubleString)

test84d = [1 .. 57] `pipe` fmap (negate `compLR` show `compLR` doubleString `compLR` doubleString)

doubleString :: String -> String
doubleString s = s ++ s

--`pipe` intercalate ":"

infixl 9 `fscomp`

fscomp :: (a -> b) -> (b -> c) -> a -> c
a `fscomp` b = b . a

infixl 9 .>>

(.>>) :: (a -> b) -> (b -> c) -> a -> c
a .>> b = b . a

tripleShow = (* 3) .>> show

--- test83b, test83a, test84a, test84b, test84c, doubleString, fscomp, pipe, pipe_old