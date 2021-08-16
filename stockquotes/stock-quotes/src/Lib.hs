module Lib where

--   ( someFunc,
--   )

import qualified Colonnade as COL
import qualified Control.Monad as MO
--import Fmt

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as DCV
import qualified Data.Foldable as DF
import Data.Function
import qualified Data.Ord as DO
import qualified Data.Text as DText
import qualified Data.Time as DTime
import qualified Fmt as FMT
  ( Buildable (..),
    Builder,
    fixedF,
    pretty,
    (+|),
    (+||),
    (|+),
    (||+),
  )
import qualified GHC.Generics as GEN
import qualified Graphics.Rendering.Chart.Backend.Diagrams as BD
import Graphics.Rendering.Chart.Easy hiding (bars, close, label)
import qualified Options.Applicative as QApp
import qualified QuoteData as QD
import qualified StatReport as SR
import qualified Text.Blaze.Colonnade as TBC
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--data Person = Person String String Int

-- person :: Person = Person "Bob" "Smith" 25

data PersonT = PersonD {firstName :: String, lastName :: String, age :: Int} deriving (Show)

-- import qualified Data.ByteString.Lazy as BL (readFile, writeFile)

person :: PersonT = PersonD {firstName = "Bob", lastName = "Smith", age = 55}
