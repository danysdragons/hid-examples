module Helpers
  ( (|>),
    module Data.Function,
    --filterOut,
    listComp,
    test70,
    test71,
    listComp2,
    test72,
  )
where

import Data.Function ((&))
--import qualified Data.Text as DT

import qualified Data.List as DL
import qualified Data.STRef as STR
import qualified Data.Text as DT
import qualified Text.Read as TR

(|>) :: a -> (a -> b) -> b
a |> b = a & b

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut pred = filter (not . pred)

listComp = [x * x | x <- [1 .. 50], condition x]

condition num = (even num && num /= 8) || num == 17

test70 = (ceiling . sqrt . fromIntegral) <$> listComp

test71 = ceiling . sqrt . fromIntegral <$> listComp & fmap (* 2)

listComp2 = [(x, y) | x <- [1 .. 50], even x, y <- [200 .. 225], odd y]

test72 = DL.groupBy (\(f1, s1) (f2, s2) -> f1 == f2) listComp2

doubleStrNumber1 :: String -> Maybe Double
doubleStrNumber1 str =
  case (TR.readMaybe str :: Maybe Double) of
    Just x -> Just (x * x)
    Nothing -> Nothing

--doubleStrNumber2:: String -> Maybe Double
doubleStrNumber2 :: (Read a, Num a) => String -> Maybe a
doubleStrNumber2 str =
  case TR.readMaybe str of
    Just x -> Just (x * x)
    Nothing -> Nothing

blah = doubleStrNumber4
  where
    doubleStrNumber4 (Just x) = Just (x * x)
    doubleStrNumber4 Nothing = Nothing

doubleStrNumber3 :: String -> Maybe Double
doubleStrNumber3 str = (* 2) <$> TR.readMaybe str

takesTuple thing@(x, y, z) = (show x, show y, show z, thing)

doubleStrNumber5 str = case TR.readMaybe str of (Just x) -> Just (x * x); Nothing -> Nothing
