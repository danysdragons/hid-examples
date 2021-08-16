module Lib where

--   ( someFunc,
--   )

import qualified Data.Sort as DS

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data MyList a = Nil | Cons a (MyList a) deriving (Show)

type MyListInt = MyList Int

testCons :: MyListInt
testCons = Nil

testCons1 :: MyListInt
testCons1 = Cons 1 Nil

testCons2 :: MyListInt
testCons2 = Cons 2 (Cons 1 Nil)

testCons3 :: MyListInt
testCons3 = Cons 3 (Cons 2 (Cons 1 Nil))

testCons3' :: MyList Int
testCons3' = testCons3

foldr11 :: (a -> b -> b) -> b -> [a] -> b
foldr11 = foldr

foldr22 :: (a -> b -> b) -> b -> [a] -> b
foldr22 f z aList = case aList of
  [] -> z
  (x : xs) -> f x (foldr22 f z xs)

showFoldFuncR2 =
  foldr22
    ( \next accum ->
        concat ["(", next, "+", accum, ")"]
    )
    "0"
    (map show [1 .. 5])

foldl11 :: (b -> a -> b) -> b -> [a] -> b
foldl11 f acc [] = acc
foldl11 f acc (x : xs) = foldl f (f acc x) xs

foldl22 :: (b -> a -> b) -> b -> [a] -> b
foldl22 f acc aList = case aList of
  [] -> acc
  (x : xs) -> foldl22 f (f acc x) xs

showFoldFuncL2 =
  foldl22
    ( \accum next ->
        concat ["(", accum, "+", next, ")"]
    )
    "0"
    (map show [1 .. 5])

foldFunc :: Int -> String -> String
foldFunc next accum = show (next * 2) ++ "-" ++ accum

foldFuncR :: Int -> String -> String
foldFuncR next accum = show (next * 2) ++ "-" ++ accum

foldFuncL :: String -> Int -> String
foldFuncL accum next = accum ++ "-" ++ show (next * 2)

testBlah xs = (x, y, z)
  where
    bl@(x : y : z) = xs

test1a = foldr foldFunc "" [1 .. 10]

test1b = scanr foldFunc "" [1 .. 10]

test2a = foldl foldFuncL mempty [1 .. 10]

test2b = scanl foldFuncL mempty [1 .. 10]

-- data Animal a = Animal a String String

myLetters = ['A' .. 'Z']

cartProd1 xs ys = [(x, y) | x <- xs, y <- ys]

cartProd2 :: [a] -> [a] -> [[a]]
cartProd2 xs ys = [[x, y] | x <- xs, y <- ys]

cartProdTest1 = cartProd1 myLetters myLetters

cartProdTest2 = cartProd2 myLetters myLetters

--getImageTitle :: String -> Int -> [(Int, String)]
getImageTitle :: String -> Int -> [(Int, String)]
getImageTitle titlePrefix highestImageNumber = idList3
  where
    myLetters = ['A' .. 'Z']
    idList1 = cartProd2 myLetters myLetters
    idList2 = take highestImageNumber $ fmap (titlePrefix ++) idList1
    idList3 = zip [1 .. highestImageNumber] idList2

displayImageTitles1 :: String -> Int -> IO ()
displayImageTitles1 titlePrefix highestImageNumber = do
  let titleList = getImageTitle titlePrefix highestImageNumber
  mapM_ (print) titleList

displayImageTitles2 :: String -> Int -> IO ()
displayImageTitles2 titlePrefix highestImageNumber = do
  let titleList = getImageTitle titlePrefix highestImageNumber
  mapM_ print titleList

displayImageTitles3 :: String -> Int -> IO ()
displayImageTitles3 titlePrefix highestImageNumber = do
  let titleList = getImageTitle titlePrefix highestImageNumber
  mapM_ print titleList

displayImageTitles4 :: String -> Int -> IO ()
displayImageTitles4 titlePrefix highestImageNumber =
  mapM_ print $ getImageTitle titlePrefix highestImageNumber

spaces :: Int -> String
spaces num = replicate num ' '

displayImageTitles5 :: String -> Int -> IO ()
displayImageTitles5 titlePrefix highestImageNumber = do
  let titleList1 = getImageTitle titlePrefix highestImageNumber
  let titleList2 = fmap (\(x, y) -> show x ++ spaces 10 ++ y ++ spaces 10) titleList1
  mapM_ putStrLn titleList2

-- MetArt_Disarm_Sophia-E_med_0061
--

displayImageTitles6 :: String -> Int -> IO ()
displayImageTitles6 titlePrefix highestImageNumber = do
  let titleList1 = getImageTitle titlePrefix highestImageNumber
  let titleList2 = fmap (\(x, y) -> show x ++ spaces 10 ++ y ++ spaces 10) titleList1
  mapM_ (\x -> putStrLn (x ++ "\n")) titleList2

-- tupListNoPrefix = zip [1 .. highestImageNumber] (take highestImageNumber idList)
-- tupListWithTuple = fmap (\ (index, identifier) -> (index, titlePrefix ++ identifier))
-- calculate = [(2, "fds")]

displayImageTitles7 :: String -> Int -> IO ()
displayImageTitles7 titlePrefix highestImageNumber = do
  let titleList1 = getImageTitle titlePrefix highestImageNumber
  let titleList2 = fmap (\(x, y) -> show x ++ spaces 10 ++ y ++ spaces 10) titleList1
  mapM_ (\x -> putStrLn (x ++ "\n")) titleList2

---

formatImageTitles :: String -> Int -> [(Int, String)]
formatImageTitles titlePrefix highestImageNumber = idList4
  where
    myLetters = ['A' .. 'Z']
    idList1 = cartProd2 myLetters myLetters
    idList2 = take highestImageNumber $ fmap (titlePrefix ++) idList1
    idList3 = zip [1 .. highestImageNumber] idList2
    idList4 = fmap (\(x, y) -> (x, y ++ "-" ++ show x)) idList3

printFormattedImageTitles :: [(Int, String)] -> IO ()
printFormattedImageTitles resultList = do
  let resultsStringified = fmap (\(x, y) -> show x ++ spaces 10 ++ y ++ spaces 10) resultList
  mapM_ (\x -> putStrLn (x ++ "\n")) resultsStringified

formattedTitles = formatImageTitles "Ava" 100

printed1 = printFormattedImageTitles formattedTitles

printed3 :: String -> Int -> IO ()
printed3 prefix highest =
  let formatted = formatImageTitles prefix highest
   in printFormattedImageTitles formatted

printed2 :: String -> Int -> IO ()
printed2 titlePrefix highestImageNumber = printFormattedImageTitles (formatImageTitles titlePrefix highestImageNumber)

something :: String -> Int -> IO ()
something prefix num = printFormattedImageTitles (formatImageTitles prefix num)

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print

--alphaNumMapping :: Int -> [(Int, String)]
alphaNumMapping num =
  let asNums = [1 .. num]
   in let asStrings = fmap show asNums
       in let tupleResults1 = zip asNums asStrings
           in let sortedStrings = DS.sort asStrings
               in let tupleResults2 = zip asNums sortedStrings
                   in (tupleResults1, tupleResults2)

printTest num = do
  let (first, second) = alphaNumMapping num
  printList first
  mapM_ (\_ -> print " ") [1 .. 5]
  printList second

--in zip asNums asStrings
