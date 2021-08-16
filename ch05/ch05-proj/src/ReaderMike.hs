{-# LANGUAGE NamedFieldPuns #-}

module ReaderMike where

-- ( ConfigM,
--   Config,
--   getConfiguration,
--   main,
--   work,
--   doSomething,
--   doSomethingSpecial,
--   beVerbose,
--   silent,
--   doSomethingSpecialSilently,
--   funcsComposed,
--   testNums,
--   multiplierFuncsComposed,
--   testCases,
--   testCases2,
--   testCases3,
--   lambada,
--   lambada2,
--   lambada3,
-- )

import Control.Monad.Reader
import Data.Function

data Config = Config
  { verbose :: Bool
  {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config {verbose = True {- ... -}}

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result

work :: ConfigM ()
work = do
  -- ...
  doSomething

-- ...

doSomething :: ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial

-- ...

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask
  vrb <- asks verbose
  when vrb beVerbose

-- ...

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

testNums :: [String]
testNums = fmap show [1 .. 20]

func1 :: String -> Int
func1 = read

func2 :: Int -> Int
func2 = (* 3)

func3 :: Int -> Double
func3 = fromIntegral

func4 :: Double -> Double
func4 n = n + 1.5

func5 :: Double -> String
func5 = show

funcsComposed :: String -> String
funcsComposed = func5 . func4 . func3 . func2 . func1

double n = n * 2

triple n = n * 3

quadruple n = n * 4

quintuple n = n * 5

multiplierFuncs :: [Int -> Int]
multiplierFuncs = [double, triple, quadruple, quintuple]

funcsCompose :: [a -> a] -> (a -> a)
funcsCompose = foldr (.) id

multiplierFuncsComposed :: Int -> Int
multiplierFuncsComposed = foldr (.) id multiplierFuncs

testCases :: Int -> String
testCases = \case
  1 -> "one"
  2 -> "two"
  3 -> "three"
  _ -> "some other number"

testCases2 :: Int -> Int -> String
testCases2 = \x y -> case (x, y) of
  pat@(1, 1) -> show pat ++ " " ++ "alpha"
  pat@(1, 2) -> show pat ++ " " ++ "beta"
  _ -> "wut?"

testCases3 :: Int -> Int -> String
testCases3 x y =
  let silly tup str2 = show tup ++ " " ++ str2
   in case (x, y) of
        pat@(1, 1) -> silly pat "alpha"
        pat@(1, 2) -> silly pat "beta"
        pat@(1, 3) -> silly pat "gamma"
        pat@(1, 4) -> silly pat "delta"
        _ -> "wut?"

lambada :: Int -> Int -> Int
lambada = curry (\(x, y) -> x + y)

lambada2 :: (Int, Int) -> Int
lambada2 = uncurry (+)

lambada3 :: Integer -> Integer -> Integer
lambada3 = (+)

wootwoot = "woot"

repped3 = replicate 15 "abc" & concat