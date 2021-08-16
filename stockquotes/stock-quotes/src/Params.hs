module Params (Params (..), cmdLineParser, cmdLineParserPure, mikeResults) where

import Data.Function
import Data.Text (Text, strip)
import Options.Applicative

data Params = Params
  { fname :: FilePath,
    company :: Maybe Text,
    chart :: Bool,
    htmlFile :: Maybe FilePath,
    silent :: Bool
  }

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument
      (metavar "FILE" <> help "CSV file name")
    <*> optional
      ( strip
          <$> strOption
            ( long "name" <> short 'n'
                <> help "Company name "
            )
      )
    <*> switch
      ( long "chart" <> short 'c'
          <> help "Generate chart"
      )
    <*> optional
      ( strOption $
          long "html" <> metavar "FILE"
            <> help "Generate HTML report"
      )
    <*> switch
      ( long "silent" <> short 's'
          <> help "Don't print statistics"
      )

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "Stock quotes data processing")

mikePrefs :: ParserPrefs
mikePrefs =
  ParserPrefs
    { prefMultiSuffix = "",
      prefDisambiguate = False,
      prefShowHelpOnError = False,
      prefShowHelpOnEmpty = False,
      prefBacktrack = True,
      prefColumns = 80
    }

cmdLineParserPure :: ParserResult Params
cmdLineParserPure = execParserPure mikePrefs opts ["data/quotes.csv", "--chart", "--name", "MichaelSoft"]
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "Stock quotes data processing")

mikeResults :: Options.Applicative.ParserResult Params -> (String, String, String)
mikeResults (Options.Applicative.Success pa) = res
  where
    res = (fname pa, show $ chart pa, show $ company pa)
mikeResults _ = ("", "", "")

-- data PersonName = PersonName {firstName :: Text, midName :: Text, lastName :: Text, talent :: Int}

-- aPerson :: PersonName
-- aPerson = PersonName {firstName = "Michael", midName = "Cameron", lastName = "Hamel", talent = 10}

-- myParams = Params {fname = "C:/file.csv", company = Just "Microsoft", chart = False, htmlFile = Just "C:/myfile.html", silent = False}

-- mikeResult :: Options.Applicative.ParserResult Params -> Maybe Params
-- mikeResult (Options.Applicative.Success pa) = Just pa
-- mikeResult _ = Nothing

-- cmdLineParserPure & extractResults

-- let temp1 = mikeResult pa in
--   let temp2 = case temp1 of Nothing -> ("", "", ""); ()