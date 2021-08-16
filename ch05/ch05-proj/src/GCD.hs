module GCD where

-- ( gcd',
--   gcdM,
--   gcdPrint,
--   gcdCountSteps,
--   gcdLogSteps,
--   gcdCountSteps',
--   gcdCountSteps'',
--   main,
--   testMonads,
-- )

import Control.Monad.Writer
import Data.Text.Internal.Builder.Int.Digits (digits)
import qualified Numeric as Nu
import Text.Read

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

summer' :: Integral a => a -> a -> a
summer' a b = foldr (+) 0 [a .. b]

-- summerM :: (Integral a, Monad m) => (a -> a -> m ()) -> a -> a -> m a
-- summerM

gcdM :: (Integral a, Monad m) => (a -> a -> m ()) -> a -> a -> m a
gcdM step a 0 = step a 0 >> pure a
gcdM step a b = step a b >> gcdM step b (a `mod` b)

gcdPrint :: (Show a, Integral a) => a -> a -> IO a
gcdPrint = gcdM (\a b -> print (a, b))

gcdPrintAlt :: (Show a, Integral a) => a -> a -> IO a
gcdPrintAlt = gcdM (curry print)

gcdCountSteps :: Integral a => a -> a -> Writer (Sum Int) a
gcdCountSteps = gcdM (\_ _ -> tell $ Sum 1)

gcdLogSteps :: Integral a => a -> a -> Writer [(a, a)] a
gcdLogSteps = gcdM (\a b -> tell [(a, b)])

gcdCountSteps' :: Integral a => a -> a -> Writer (Sum Int) a
gcdCountSteps' a b = mapWriter mapper (gcdLogSteps a b)
  where
    mapper (v, w) = (v, Sum $ length w)

gcdCountSteps'' :: Integral a => a -> a -> Writer (Sum Int) a
gcdCountSteps'' = (mapWriter (Sum . length <$>) .) . gcdLogSteps

testMonads :: Int -> Int -> IO ()
testMonads num1 num2 = do
  print $ "testing with: " ++ " num1 = " ++ show num1 ++ ", num2 = " ++ show num2
  print "gcd' Testing: "
  let gcdTestResults1 = gcd' num1 num2
  print "gcdM Testing: "
  gcdTestResults2 <- gcdM (\x y -> print (x, y)) num1 num2
  gcdTestResults3 <- gcdM (curry print) num1 num2
  print gcdTestResults2

--print gcdTestResults

-- putStrLn $ show gcdTestResults
--print gcdTestResults

main :: IO ()
main = print "OK"

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct list1 list2 = [(x, y) | x <- list1, y <- list2]

res = cartProduct [1 .. 25] [1 .. 25]

getGCDs1 = fmap (uncurry gcd') res

getGCDs2 :: [((Integer, Integer), Integer)]
getGCDs2 = fmap (\(x, y) -> ((x, y), gcd' x y)) res

--getGCDs3 :: [Int] -> [Int] -> [((Int, Int), Int)]
getGCDs3 :: (Integral b, Show b) => [b] -> [b] -> [((b, b), b)]
getGCDs3 list1 list2 = fmap (\(x, y) -> ((x, y), gcd' x y)) res1
  where
    res1 = cartProduct list1 list2

printList :: (Show a) => [a] -> IO ()
printList lists = do
  mapM_ print lists

printTest1 :: [Int] -> [Int] -> IO ()
printTest1 li l2 = printList (getGCDs3 li li)

listUpTo :: (Num a, Enum a) => a -> [a]
listUpTo num = [1 .. num]

printTest2 :: [Int] -> [Int] -> IO ()
printTest2 li l2 = printList (getGCDs3 li li)

numberToWord :: Int -> String
numberToWord 1 = "one"
numberToWord 2 = "two"
numberToWord 3 = "three"
numberToWord 4 = "four"
numberToWord 5 = "five"
numberToWord 6 = "six"
numberToWord 7 = "seven"
numberToWord 8 = "eight"
numberToWord 9 = "nine"
numberToWord 10 = "ten"
numberToWord 11 = "eleven"
numberToWord 12 = "twelve"
numberToWord 13 = "thirteen"
numberToWord 14 = "fourteen"
numberToWord 15 = "fifteen"
numberToWord 16 = "sixteen"
numberToWord 17 = "seventeen"
numberToWord 18 = "eighteen"
numberToWord 19 = "nineteen"
numberToWord 20 = "twenty"
numberToWord _ = "a lot"

mathExpToWords :: Int -> Int -> String
mathExpToWords x y = toWords
  where
    arithmeticResult = x + y
    toWords =
      if arithmeticResult <= 20
        then numberToWord x ++ " plus " ++ numberToWord y ++ " is " ++ numberToWord arithmeticResult
        else "I can't handle numbers that big!"

lotsOfExpressions :: Int -> [String]
lotsOfExpressions numLimit = fmap (uncurry mathExpToWords) allCombinations
  where
    aList = [1 .. numLimit]
    allCombinations = cartProduct aList aList

data Person = Person Int Int

sillyYou1 = printList $ reverse $ fmap reverse $ lotsOfExpressions 5

sillyYou2 = printList $ reverse $ reverse <$> lotsOfExpressions 5

test1 :: [String]
test1 = lotsOfExpressions 5

test2 :: [String]
test2 = reverse <$> lotsOfExpressions 5

test5 :: [String]
test5 = reverse $ reverse <$> lotsOfExpressions 5

may :: Maybe Int = readMaybe "8"

-- reverse <%> lotsOfExpressions 5

doubleStrNumber123 str =
  case readMaybe str of
    Just x -> Just (x * 2)
    Nothing -> Nothing

doubleStrNumber124 str =
  case readMaybe str of
    (Just x) -> Just (x * 2)
    Nothing -> Nothing

numYears :: Int = 50

daysPerYear :: Int = 365

hoursPerDay :: Int = 24

minutesPerHour :: Int = 60

secondsPerMinute = 60

secondsSince1970 :: Int
secondsSince1970 = numYears * daysPerYear * hoursPerDay * minutesPerHour * secondsPerMinute

repli :: Int -> Int -> [Int] = replicate

someTwos :: Int -> [Int]
someTwos num = repli num 2

blah = foldr (*) 1 (someTwos 32)

blah1 = foldl (+) 1 (someTwos 32)

plusStrNumbers' :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers' s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

plusStrNumbers1 :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers1 s1 s2 = (fmap (+) (readMaybe s1)) <*> readMaybe s2

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]
