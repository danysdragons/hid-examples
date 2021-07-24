{-# LANGUAGE TupleSections #-}

module Contexts where

import Control.Monad.Writer (MonadWriter (tell, writer), Writer)
import Data.Function
import qualified Data.List as DL
import Radar (CyclicEnum (csucc), Direction (North))

readNumber :: IO Int
readNumber = do
  s <- getLine
  pure (read s)

sumN :: Int -> Writer String Int
sumN 0 = writer (0, "finish")
sumN n = do
  tell (show n ++ ",")
  s <- sumN (n -1)
  pure (n + s)

cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure (x, y)

addNumber :: Int -> IO String
addNumber n = pure (++) <*> pure (show n ++ " ") <*> getLine

test555 = traverse addNumber [1 .. 5]

-- ((++) <$> pure (show n ++ " "))
tata4 :: [Integer]
tata4 = enumFrom 4

-- enumFrom4 :: [Integer]
-- enumFrom4 = [4,5,6,7...]

zippy1 = zip [1 .. 100] [100, 99 .. 1]

zippy2 = fmap (\(f, s) -> (s, f)) zippy1

zippy3 = fmap (\(f, s) -> f + s) zippy1

zippy4 = fmap (uncurry (+)) zippy1

zippy5 = fmap (uncurry (-)) zippy1

uncMinus = uncurry (-)

zippy6 = fmap (\(f, s) -> s - f) zippy1

--zippy6 = fmap  (

tupApp f (first, second) = (f first, f second)

vv1 :: Maybe (Int, [Int])
vv1 = DL.uncons [1 .. 20]

r1 :: [(Int, Int)]
r1 = case vv1 of Nothing -> []; Just (blah, values) -> fmap (\x -> (blah, x)) values

(Just val) = vv1

test1 Nothing = []
test1 (Just aList) = aList

a :: [Integer]
a = scanl (+) 0 [1 .. 10]

a122 = scanr (+) 0 [1 .. 10]

a222 = scanr (:) [] [1 .. 10]

a333 = scanr folder [] [1 .. 10]
  where
    folder = (:)

scanTest1 = scanr folder [] [1 .. 10]
  where
    folder = foldFunc

scanTest2 :: [[(Int, String)]]
scanTest2 = [[(3, "")]]

createTupled1 :: Int -> [(Int, String)]
createTupled1 num =
  let temp1 = [1 .. num]
   in let temp2 = fmap (\x -> (x, show x)) temp1
       in temp2

createTupled2 :: Int -> [(Int, String)]
createTupled2 num =
  let temp1 = [1 .. num]
   in let temp2 = fmap (,mempty) temp1
       in temp2

testFoldFunc2 =
  let temp1 = createTupled1 5
   in let temp2 = scanr foldFunc2 [(1, "")] temp1
       in temp2

foldFunc2 :: (Int, String) -> [(Int, String)] -> [(Int, String)]
foldFunc2 (nextInt, nextS) accumList = (nextInt, helper4) : accumList
  where
    helper1 = "Next value to fold: " ++ "(" ++ show nextInt ++ ", " ++ nextS ++ " )"
    helper2 = "Accumulated so far: " ++ show accumList
    helper3 = helper1 ++ "; " ++ helper2
    helper4 = "placeholder"

foldFunc :: Int -> [Int] -> [Int]
foldFunc next accum = next : accum

help = "(" ++ show nextInt ++ ", " ++ nextS ++ ")"
  where
    nextInt = 4
    nextS = "hello"

folded = take 10 $ repeat [(.)]

type DirectFunc = Direction -> Direction

folded2 = foldr (.) (id :: DirectFunc) (replicate 13 Radar.csucc) North

enumResult = toEnum 3 :: Direction

flippedApp :: a -> (a -> c) -> c
flippedApp = flip ($)

-- -> (f a -> (a -> b) -> f b)

flippedFMap :: (Functor f) => f a -> (a -> b) -> f b
flippedFMap values func = fmap func values

testBlah = (1 + 3 + 5) `flippedApp` negate

someValue =
  let person1 = "Michael"
   in let person2 = "Jane"
       in let fuckReport = person1 ++ " fucks " ++ person2 in fuckReport

someValue2 :: String -> String -> String
someValue2 p1 p2 = formattedPerson1 ++ " fucks " ++ formattedPerson2
  where
    formattedPerson1 = p1 ++ " himself!!!"
    formattedPerson2 = p2 ++ " herself!!!"

t11 = filter even [1 .. 100]

t12 = tail $ zip (0 : [1 .. 100]) [1 .. 100]

t13 = zip (0 : [1 .. 100]) [1 .. 100] & tail

t14 :: Int -> [(Int, Int)]
t14 num = zip (0 : [1 .. num]) [1 .. num] & tail

t15 :: Int -> [Int]
t15 num = zip (0 : [1 .. num]) [1 .. num] & tail & fmap fst

t16 :: Int
t16 = (\x y -> x + y) 4 5

t17 = zip [1 .. 100] [-1, -2 .. -100]

tuppend :: (a, b) -> c -> (a, b, c)
tuppend (x, y) z = (x, y, z)

tuppend' = flip tuppend

tuppendA = tuppend' "A"

ttt = flip tuppend "A"

tttt = flip tuppend "A" <$> t17

t8 :: [(Int, Int, Int)]
t8 = fmap (\(f, s) -> (f, s, 0)) t17

t9 :: [(Int, Int)] -> [(Int, Int, Int)]
t9 num = fmap funky num
  where
    funky = (\(f, s) -> (f, s, 0))

funky2 = (\(f, s) -> (f, s, 0))

testData = t9 t17

--testData2 = fmap (flip funky2) testData

-- a |> b = flip ($)

-- t13 :: [Int]
-- t13 = [1 .. 50] & fmap (+1)