-- Copyright 2022 Isaac Van Doren

module Cook (Result(..), Content(..), Category(..), Item(..), Step, Metadata, parseCook) where

import Text.Megaparsec (optional, some, noneOf, many, errorBundlePretty, parse, MonadParsec(try), (<|>), Parsec)
import Text.Megaparsec.Char (hspace, char, string, space, digitChar, newline)
import Data.Void (Void)
import Control.Monad (void, when)
import Text.Printf (printf)

type Parser = Parsec Void String

type Metadata = (String, String)

type Step = [Content]
data Content = Text String
    | Ingredient String String String   -- label quantity units
    | Timer String String String        -- label quantity units
    | Cookware String String            -- label quantity
    deriving (Show, Eq)

data Item = Item String String deriving (Show, Eq)
data Category = Category String [Item] deriving (Show, Eq)

data Result = Recipe [Metadata] [Step] | Grouping [Category]
    deriving (Show, Eq)

instance Semigroup Result where
    (Recipe m s) <> (Recipe m' s') = Recipe (m ++ m') (s ++ s')
instance Monoid Result where
    mempty = Recipe [] []


parseCook :: String -> Either String Result
parseCook input = case parse cookFile "" (simplify input) of
    Left bundle -> Left $ errorBundlePretty bundle
    Right result -> Right result

simplify :: String -> String
simplify = inlineComments . blockComments
    where
        inlineComments = unlines . map f . lines
            where
                f ('-':'-':_) = ""
                f (x:xs) = x : f xs
                f "" = ""
        blockComments :: String -> String
        blockComments ('[':'-':xs) = blockComments (consume xs)
            where 
                consume ('-':']':xs) = xs
                consume (_:xs) = consume xs
                consume "" = ""
        blockComments (x:xs) = x : blockComments xs
        blockComments "" = ""

cookFile :: Parser Result
cookFile = grouping <|> fmap mconcat (space *> many (try metadata <* space <|> step <* space))

grouping :: Parser Result
grouping = Grouping <$> some (category <* many newline)

category :: Parser Category
category = do
    title <- hspace *> char '[' *> some (noneOf "]") <* char ']' <* hspace <* optional newline
    items <- many item
    return $ Category (norm title) items

item :: Parser Item
item = do
    first <- some (noneOf "\n|")
    second <- optional (char '|') *> many (noneOf "\n") <* newline
    return $ Item (norm first) (norm second)

metadata :: Parser Result
metadata = do
    void $ string ">>" <* hspace
    key <- some $ noneOf ":"
    void $ char ':' <* hspace
    value <- some $ word <* hspace
    return $ Recipe [(norm key, unwords value)] []

step :: Parser Result
step = do
    content <- some $ (ingredient <|> cookware <|> timer <|> text)
    return $ Recipe [] [content]

text :: Parser Content
text = Text <$> (some (noneOf "@#~\n"))

ingredient :: Parser Content
ingredient =  char '@' *> hspace *>
    (try (do
        content <- some $ noneOf "#~@\n{"
        void $ char '{'
        (amount, units) <- quantity
        void $ char '}'
        return $ Ingredient (norm content) (case amount of "" -> "some"; x -> x) units)
    <|> fmap (\x -> Ingredient x "some" "") word)

cookware :: Parser Content
cookware = char '#' *> hspace *>
    (try (do
        content <- some $ noneOf "#~@\n{"
        (amount, _) <- char '{' *> quantity <* char '}'
        return $ Cookware (norm content) (case amount of "" -> "1"; x -> x))
    <|> fmap (\x -> Cookware x "1") word)

timer :: Parser Content
timer = char '~' *> hspace *>
    (try (do
        timerLabel <- many $ noneOf "#~@\n{"
        void $ char '{'
        (amount, units) <- quantity
        void $ char '}'
        return $ Timer (norm timerLabel) amount units)
    <|> fmap (\x -> Timer x "" "") word)

quantity :: Parser (String, String)
quantity = do
    amount <- try fraction <|> many (noneOf "\n%}")
    void $ hspace *> optional (char '%') <* hspace
    units <- many $ noneOf "\n}"
    return (norm amount, norm units)

fraction :: Parser String
fraction = do
    n1 <- some digitChar <* hspace <* char '/' <* hspace
    n2 <- some digitChar
    when (all (== '0') n2 || head n1 == '0') (fail "not a vaild fraction")
    return $ let x = printf "%.2f" ((read n1 / read n2) :: Double)
            in if last x == '0' then init x else x

word :: Parser String
word = some $ noneOf " \t\n"

-- removes extra whitespace
norm :: String -> String
norm = unwords . words
