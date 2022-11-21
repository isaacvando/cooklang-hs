-- Copyright 2022 Isaac Van Doren

module Cook (Recipe(..), Content(..), Step, Metadata, parseCook) where

import Text.Megaparsec (optional, some, noneOf, many, errorBundlePretty, parse, MonadParsec(try, notFollowedBy), (<|>), Parsec, oneOf)
import Text.Megaparsec.Char (hspace, char, string, space, digitChar)
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
data Recipe = Recipe [Metadata] [Step]
    deriving (Show, Eq)

instance Semigroup Recipe where
    (Recipe m s) <> (Recipe m' s') = Recipe (m ++ m') (s ++ s')
instance Monoid Recipe where
    mempty = Recipe [] []
    

parseCook :: String -> Either String Recipe
parseCook input = case parse cookFile "" input of
    Left bundle -> Left $ errorBundlePretty bundle
    Right result -> Right result

cookFile :: Parser Recipe
cookFile = fmap mconcat (many $ (try comment <|> try metadata <|> step) <* space)

comment :: Parser Recipe
comment = do
    void $ string "--" *> many (noneOf "\n")
        <|> string "[-" *> many (try (char '-' <* notFollowedBy (char ']')) <|> noneOf "-") *> string "-]"
    return mempty

metadata :: Parser Recipe
metadata = do
    void $ string ">>" <* hspace
    key <- some $ noneOf ":"
    void $ char ':' <* hspace
    value <- some $ word <* hspace <* optional comment
    return $ Recipe [(norm key, unwords value)] []

step :: Parser Recipe
step = do
    content <- some $ (ingredient <|> cookware <|> timer <|> text) <* optional comment
    return $ Recipe [] [content]

text :: Parser Content
text = do
    -- this use of notFollowedBy prevents comments from being parsed as text
    t <- some $ try (char '-' <* notFollowedBy (oneOf "-]"))
        <|> try (char '[' <* notFollowedBy (char '-'))
        <|> noneOf "@#~\n-["
    return $ Text t

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
