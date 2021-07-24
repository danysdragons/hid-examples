module Main where

import Charts
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
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

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)

quoteFilePath :: FilePath
quoteFilePath = "/home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data/quotes.csv"

-- /home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data
-- /home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes/data/quotes.csv