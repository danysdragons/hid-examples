module Main where

import Charts
import Control.Monad (unless, when)
import Data.ByteString.Internal (ByteString)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (Header, decodeByName)
import Data.Foldable (toList)
import Data.Function
import Data.Text (unpack)
import qualified Data.Vector as DV
import Graphics.Rendering.Chart.Geometry (Vector)
import Helpers
import HtmlReport
import Params
import QuoteData
import StatReport

generateReports ::
  (Functor t, Foldable t) =>
  Params ->
  t QuoteData ->
  IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

    withCompany prefix = maybe mempty (prefix <>) company
    chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

main :: IO ()
main = cmdLineParser >>= work

mainParseOnly :: IO Params
mainParseOnly = cmdLineParser

readQuotes2 :: FilePath -> IO String
readQuotes2 fpath = do
  csvData <- BL.readFile fpath
  let decodedData :: Either String (Header, DV.Vector QuoteData) = decodeByName csvData
  let vectorQuoteData = case decodedData of
        Left err -> DV.empty
        Right (_, quotes) -> quotes
  let aReport1 =
        if DV.length vectorQuoteData > 0
          then statInfo vectorQuoteData & textReport
          else "Report could not be generated"
  return aReport1

readQuotes2a :: FilePath -> IO String
readQuotes2a fpath = do
  csvData <- BL.readFile fpath
  let decodedData :: Either String (Header, DV.Vector QuoteData) = decodeByName csvData
  let vectorQuoteData = case decodedData of
        Left err -> DV.empty
        Right (_, quotes) -> quotes
  return $
    if DV.length vectorQuoteData > 0
      then statInfo vectorQuoteData `pipe` textReport
      else "Report could not be generated"

readQuotes2aa :: FilePath -> IO String
readQuotes2aa fpath = do
  csvData <- BL.readFile fpath
  let decodedData :: Either String (Header, DV.Vector QuoteData) = decodeByName csvData
  let vectorQuoteData = case decodedData of
        Left err -> DV.empty
        Right (_, quotes) -> quotes
  return $ statInfo vectorQuoteData `pipe` textReport

-- (return "Nope")
-- (return statInfo quotes `pipe` textReport)

readQuotes2aaa :: FilePath -> IO String
readQuotes2aaa fpath = do
  csvData <- BL.readFile fpath
  let decodedData :: Either String (Header, DV.Vector QuoteData) = decodeByName csvData
  return $ case decodedData of
    Left err -> "Regrettably, a report could not be generated :("
    Right (_, quotes) -> textReport $ statInfo quotes

readQuotes2compact :: FilePath -> IO String
readQuotes2compact fpath = do
  csvData <- BL.readFile fpath
  return $ case decodeByName csvData of
    Left err -> "Regrettably, a report could not be generated :("
    Right (_, quotes) -> textReport $ statInfo quotes

displayResults :: FilePath -> IO ()
displayResults fileName = do
  result <- readQuotes2compact fileName
  putStrLn result

displayResultsSample :: IO ()
displayResultsSample = do
  result <- readQuotes2compact "data/quotes.csv"
  putStrLn result

-- repLines = lines <$> res
-- niceOutput = (\allLines -> mapM_ putStrLn allLines) <$> repLines
-- niceOutput = (\allLines -> fmap putStrLn allLines) <$> repLines

displayResults2 :: FilePath -> IO ()
displayResults2 fileName = do
  result <- readQuotes2compact fileName
  putStrLn result

--let temp2 = result

result = readQuotes2compact "data/quotes.csv"

val = (\str -> putStrLn <$> (lines str)) <$> result

val2 = (putStrLn <$> result)

myTest = do
  res323 <- (putStrLn <$> result)
  res323

readQuotes1 :: FilePath -> IO String
readQuotes1 fpath = do
  csvData <- BL.readFile fpath
  let decodedData :: Either String (Header, DV.Vector QuoteData) = decodeByName csvData
  let temp = case decodedData of
        Left err -> DV.empty
        Right (_, quotes) -> quotes
  let aReport1 =
        if DV.length temp > 0
          then Just (statInfo temp)
          else Nothing
  let aReport2 = case aReport1 of
        (Just something) -> textReport something
        Nothing -> "No report could be generated"
  return aReport2

--- niceOutput = (\allLines -> mapM_ putStrLn allLines) <$> repLines

-- let temp = case decodeByName csvData of
--       Left err -> error err
--       Right (_, quotes) -> pure (toList quotes)
-- temp

-- --readQuotesPure :: FilePath -> Maybe [QuoteData]
-- readQuotesPure1 fpath =
--     helper
--     where helper = do
--           csvData <- BL.readFile
--           return csvData

-- --readQuotesPure2 :: p -> IO Data.ByteString.Lazy.Internal.ByteString
-- readQuotesPure2 fpath =
--     helper
--     where
--       helper = do
--           csvData <- BL.readFile fpath
--           return csvData

-- let temp = do
-- csvData <- BL.readFile
-- let decoded =
--   case decodeByName csvData of
--     Left err -> return $ error err
--     Right (_, quotes) -> pure (toList quotes)

quoteFilePath :: FilePath
quoteFilePath = "/home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data/quotes.csv"

-- /home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data
-- /home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data/quotes.csv