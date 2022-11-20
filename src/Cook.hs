module Cook where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

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
    -- return $ Recipe [] [foldr f [] content]
    --     where
    --         f (Text st1) ((Text st2):acc) = Text (st1 ++ " " ++ st2):acc
    --         f val acc = val:acc

text :: Parser Content
text = do
    t <- some $ noneOf "@#~\n"
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

timer :: Parser Content
timer = do
    void $ char '~'
    timerLabel <- many $ noneOf "#~@\n{"
    void $ char '{'
    (amount, units) <- quantity
    void $ char '}'
    return $ Timer (norm timerLabel) amount units

quantity :: Parser (String, String)
quantity = do
    amount <- many $ noneOf "\n%}"
    void $ optional (char '%')
    unit <- many $ noneOf "\n}"
    return (norm amount, norm unit)

word :: Parser String
word = some $ noneOf " \t\n"
    -- endSpace <- optional $ char ' '
    -- return $ chars ++ (case endSpace of Nothing -> ""; Just _ -> " ")

-- removes extra whitespace
norm :: String -> String
norm = unwords . words
