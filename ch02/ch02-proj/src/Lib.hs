module Lib
  ( someFunc,
  --hiding NEL
  )
where

import qualified Data.List.NonEmpty as NEL
import Data.Semigroup

-- type NELIntList = NEL.NonEmpty Int

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add3 :: Int -> Int
add3 = (+ 3)

add4 :: Int -> Int
add4 = (+ 4)

add5 :: Int -> Int
add5 = (+ 5)

add6 :: Int -> Int
add6 = (+ 10)

type IntFunc = Int -> Int

addFuncs1 :: [Int -> Int]
addFuncs1 = [add3, add4, add5, add6]

foldedFuncs2 :: Int -> Int
foldedFuncs2 = foldr (.) id addFuncs1