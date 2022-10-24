module Cook where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

type Metadata = (String, String)
type Step = [(String, Annotation)]
data Annotation = Empty | Ingredient (Maybe String) | Timer String | Cookware String
    deriving (Show, Eq)
data Recipe = Recipe [Metadata] [Step]
    deriving (Show, Eq)


parseCook :: String -> Either String Recipe
parseCook input = case parse cookFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

cookFile :: Parser Recipe
cookFile = do
    content <- many $ try (fmap Left (metadata <* (optional $ char '\n'))) <|> (fmap Right (step <* (optional $ char '\n')))
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
    value <- sentence
    return (key, value)

step :: Parser Step
step = do
    content <- some (hspace >> (ingredient <|> stepWord) <* hspace)
    return $ foldr f [] content
        where 
            f val [] = [val]
            f (st1, Empty) ((st2, Empty):acc) = (st1 ++ " " ++ st2, Empty):acc
            f val acc = val:acc


ingredient :: Parser (String, Annotation)
ingredient = do 
    -- val <- try (fmap concat (char '@' >> hspace >> some (word <* hspace) <* string "{}")) <|> (char '@' >> word)
    val <- try (fmap unwords (char '@' >> hspace >> some ((some $ noneOf " \t\n{") <* hspace) <* string "{}")) <|> (char '@' >> word)
    return (val, Ingredient Nothing)

stepWord :: Parser (String, Annotation)
stepWord = do
    val <- word
    return (val, Empty)
    
    
        

-- sentence insensitive to horizontal space
sentence :: Parser String
sentence = fmap unwords (hspace >> (some $ (some $ noneOf " \t\n") <* hspace))

word :: Parser String
word = some $ noneOf " \t\n"