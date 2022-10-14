module Lib ( parseCook ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
-- import qualified Data.Text as T

type Parser = Parsec Void String

parseCook :: String -> Either String String
parseCook input = case parse cookFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result


cookFile :: Parser String
cookFile = string "foo"
