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

instance Semigroup Recipe where
    (Recipe m s) <> (Recipe m' s') = Recipe (m ++ m') (s ++ s')

instance Monoid Recipe where
    mempty = Recipe [] []
    



parseCook :: String -> Either String Recipe
parseCook input = case parse cookFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

cookFile :: Parser Recipe
cookFile = fmap mconcat (many $ (try comment <|> try metadata <|> step) <* (optional $ char '\n'))

comment :: Parser Recipe
comment = do
    hspace 
    void $ string "--" 
    void $ many $ noneOf "\n"
    return mempty

metadata :: Parser Recipe
metadata = do 
    void $ string ">>"
    hspace
    key <- some $ noneOf " \t:"
    void $ char ':'
    value <- sentence
    return $ Recipe [(key, value)] []

step :: Parser Recipe
step = do
    content <- some (hspace *> (ingredient <|> stepWord) <* hspace)
    return $ Recipe [] [foldr f [] content]
        where 
            f val [] = [val]
            f (st1, Empty) ((st2, Empty):acc) = (st1 ++ " " ++ st2, Empty):acc
            f val acc = val:acc


  -- val <- try (fmap unwords (char '@' >> hspace >> some ((some $ noneOf " \t\n{") <* hspace) <* string "{}")) 
    --     <|> (char '@' >> word)
ingredient :: Parser (String, Annotation)
ingredient =  try (do
        void $ char '@'
        hspace
        content <- some ((some $ noneOf " \t\n{") <* hspace)
        void $ char '{'
        ing <- many ((some $ noneOf " \t\n}") <* hspace)
        void $ char '}'
        return (unwords content, Ingredient (if null ing then Nothing else Just $ unwords ing)))
        <|> (do
        void $ char '@'
        content <- word
        return (content, Ingredient Nothing))

stepWord :: Parser (String, Annotation)
stepWord = do
    val <- word
    return (val, Empty) 

-- sentence insensitive to horizontal space
sentence :: Parser String
sentence = fmap unwords (hspace >> (some $ (some $ noneOf " \t\n") <* hspace))

word :: Parser String
word = some $ noneOf " \t\n"