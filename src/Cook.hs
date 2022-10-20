module Cook ( parseCook, Recipe(..) ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

type Metadata = (String, String)
type Step = String
data Recipe = Recipe [Metadata] [Step]
    deriving (Show, Eq)


parseCook :: String -> Either String Recipe
parseCook input = case parse cookFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

cookFile :: Parser Recipe
cookFile = do
    -- ms <- some $ metadata <* (optional $ char '\n')
    content <- some $ try (fmap Left (metadata <* (optional $ char '\n'))) <|> (fmap Right (step <* (optional $ char '\n')))
    return $ foldr makeRecipe (Recipe [] []) content
        where
            makeRecipe (Left x) (Recipe m s) = Recipe (x:m) s
            makeRecipe (Right x) (Recipe m s) = Recipe m (x:s)


metadata :: Parser Metadata
metadata = do 
    void $ string ">>"
    hspace
    key <- some $ noneOf " \t:"
    void $ char ':'
    hspace 
    value <- some $ (some $ noneOf " \t\n") <* hspace
    return (key, unwords value)

step :: Parser Step
step = do
    value <- some $ (some $ noneOf " \t\n") <* hspace
    return $ unwords value