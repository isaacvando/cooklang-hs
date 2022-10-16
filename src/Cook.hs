module Cook ( parseCook, Recipe ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

type Metadata = [(String, String)]
data Recipe = Recipe Metadata
    deriving Show

parseCook :: String -> Either String Recipe
parseCook input = case parse cookFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result


cookFile :: Parser Recipe
cookFile = return $ Recipe []
