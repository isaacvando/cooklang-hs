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
    singleComment <|> blockComment <?> "comment"
    return mempty

singleComment :: Parser ()
singleComment = hspace *> string "--" *> (many $ noneOf "\n") *> return ()

blockComment :: Parser ()
blockComment = do
    hspace
    void $ string "[-"
    void $ (many (try (char '-' <* notFollowedBy (char ']')) <|> noneOf "-"))
    void $ string "-]"
    hspace
    return ()

metadata :: Parser Recipe
metadata = do 
    void $ string ">>"
    hspace
    key <- some $ noneOf " \t:"
    void $ char ':'
    hspace
    value <- some $ word <* hspace <* (optional comment)
    return $ Recipe [(key, unwords value)] []

step :: Parser Recipe
step = do
    content <- some (hspace *> (ingredient <|> fmap (,Empty) word) <* hspace <* (optional comment))
    return $ Recipe [] [foldr f [] content]
        where 
            f val [] = [val]
            f (st1, Empty) ((st2, Empty):acc) = (st1 ++ " " ++ st2, Empty):acc
            f val acc = val:acc

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

word :: Parser String
word = some $ noneOf " \t\n"