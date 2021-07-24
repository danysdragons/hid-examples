{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GenSQL where

-- module GenSQL
--   ( SQL,
--     ErrorMsg,
--     genInsert,
--     processLine,
--     genSQL,
--     testData,
--     testGenSQL,
--     main,
--     fvp,
--     fvp2,
--     testData1,
--     testData2,
--     testOuterData1,
--     testOuterData2,
--     testOuterData3,
--     getStuffed2,
--   )
-- where

import Control.Monad.Writer
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type SQL = Text

data ErrorMsg = WrongFormat Int Text
  deriving (Show)

genInsert :: Text -> Text -> Text
genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

-- [ErrorMsg]
type List = []

processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genSQL :: Text -> Writer [ErrorMsg] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1 ..] $ T.lines txt)

testData :: Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors

main :: IO ()
main = testGenSQL

fvp :: Text -> [Text]
fvp (T.splitOn ":" -> all@(x : xs)) = all
fvp _ = []

fvp2 :: (Maybe Int, Int) -> Int
fvp2 (Just 2, n) = 2
fvp2 (Just 3, n) = 4
fvp2 (Just x, n) = wootful n
  where
    wootful x
      | x > 0 = 17
      | x == 0 = 18
      | x < 0 = 19
      | otherwise = 20
fvp2 (Nothing, _) = -1

nameAndInitials :: String -> String -> String -> (String, [Char], [Char])
nameAndInitials firstName middleName lastName = finalResult
  where
    (f : _) = firstName
    (m : _) = middleName
    (l : _) = lastName
    initialsRes = [f, m, l]
    initialsRes2 = [f] ++ "." ++ [m] ++ "." ++ [l]
    nameRes = firstName ++ " " ++ middleName ++ " " ++ lastName
    finalResult = (nameRes, initialsRes, initialsRes2)

newtype Stuff = SomeStuff (String, Maybe Int, String) deriving (Show)

getStuffed :: Stuff -> Int
getStuffed (SomeStuff (_, Just num, _)) = num
getStuffed (SomeStuff (_, Nothing, _)) = -1

data Stuff2 = SomeStuff2 Int (Maybe Stuff) Int deriving (Show)

getStuffed2 :: Stuff2 -> (Int, Int, Int)
getStuffed2 (SomeStuff2 val1 (Just blah) val2) = (val1, val2, woot)
  where
    woot :: Int = case blah of
      SomeStuff (s1, Just number, s2) -> number
      SomeStuff (s1, Nothing, s2) -> -123
getStuffed2 (SomeStuff2 val1 Nothing val2) = (val1, val2, 0)

testData1 :: Stuff = SomeStuff ("fsdf", Nothing, "fsgd")

testData2 :: Stuff = SomeStuff ("gdfa", Just 323, "asfsf")

testOuterData1 :: Stuff2
testOuterData1 = SomeStuff2 7 (Just testData1) 89

testOuterData2 :: Stuff2
testOuterData2 = SomeStuff2 8 (Just testData2) 90

testOuterData3 :: Stuff2
testOuterData3 = SomeStuff2 9 Nothing 91
