module Cook where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Text.Printf

type Parser = Parsec Void String
type Metadata = (String, String)
type Step = [Content]
data Content = Text String | Ingredient String String String | Timer String String String | Cookware String String
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
cookFile = fmap mconcat (many $ (try comment <|> try metadata <|> step) <* space)

comment :: Parser Recipe
comment = do
    _ <- string "--" *> many (noneOf "\n")
        <|> string "[-" *> many (try (char '-' <* notFollowedBy (char ']')) <|> noneOf "-") *>  string "-]"
    return mempty

singleComment :: Parser ()
singleComment = (hspace *> string "--" *> many (noneOf "\n")) *> return ()

blockComment :: Parser ()
blockComment = do
    hspace
    void $ string "[-"
    void (many (try (char '-' <* notFollowedBy (char ']')) <|> noneOf "-"))
    void $ string "-]"
    hspace
    return ()

metadata :: Parser Recipe
metadata = do
    void $ string ">>"
    hspace
    key <- some $ noneOf ":"
    void $ char ':'
    hspace
    value <- some $ word <* hspace <* optional comment
    return $ Recipe [(norm key, unwords value)] []

step :: Parser Recipe
step = do
    content <- some $ (ingredient <|> cookware <|> try timer <|> text) <* optional comment
    return $ Recipe [] [content]

text :: Parser Content
text = do
    -- this use of notFollowedBy prevents comments from being parsed as text
    t <- some $ try (char '-' <* notFollowedBy (oneOf "-]"))
        <|> try (char '[' <* notFollowedBy (char '-')) 
        <|> noneOf "@#~\n-["
    return $ Text t

ingredient :: Parser Content
ingredient =  try (do
    void $ char '@'
    hspace
    content <- some $ noneOf "#~@\n{"
    void $ char '{'
    (amount, units) <- quantity
    void $ char '}'
    return $ Ingredient (norm content) (case norm amount of "" -> "some"; x -> x) (norm units))
    <|> (do
    void $ char '@'
    content <- word
    return $ Ingredient content "some" "")

cookware :: Parser Content
cookware = char '#' *> hspace *>
    (try (do
    content <- some $ noneOf "#~@\n{"
    (amount, _) <- char '{' *> quantity <* char '}'
    return $ Cookware (norm content) (case norm amount of "" -> "1"; x -> x))
    <|> (do
    content <- word
    return $ Cookware content "1"))


-- simplify this
timer :: Parser Content
timer = try (do
        void $ char '~'
        timerLabel <- many $ noneOf "#~@\n{"
        void $ char '{'
        (amount, units) <- quantity
        void $ char '}'
        return $ Timer (norm timerLabel) amount units)
    <|> (do
        void $ char '~'
        timerLabel <- many $ noneOf "#~@\n{"
        return $ Timer (norm timerLabel) "" "")

quantity :: Parser (String, String)
quantity = do
    amount <- try fraction <|> many (noneOf "\n%}")
    void $ hspace *> optional (char '%') <* hspace
    unit <- many $ noneOf "\n}"
    return (norm amount, norm unit)

fraction :: Parser String
fraction = do
    n1 <- some digitChar <* hspace <* char '/' <* hspace
    n2 <- some digitChar
    void $ if all (== '0') n2 || head n1 == '0' then fail "not a fraction" else return "" -- there must be a better way to do this
    return $ let x = printf "%.2f" ((read n1 / read n2) :: Double)
            in if last x == '0' then init x else x

word :: Parser String
word = some $ noneOf " \t\n"

-- removes extra whitespace
norm :: String -> String
norm = unwords . words
