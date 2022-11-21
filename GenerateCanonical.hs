-- Copyright 2022 Isaac Van Doren

module GenerateCanonical where

import Cook
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
-- TODO: use Data.Yaml instead

type Parser = Parsec Void String

        -- Name Input Metadatas Steps
data Test = Test String String [Metadata] [Step]

instance Show Test where
    show (Test name input m s) = "        it \"" ++ name ++ "\" $ do\n" ++ "            parseCook \"" ++
       foldr (\x acc -> if x == '\n' then '\\':'n':acc else x:acc) [] input ++ "\" `shouldBe` Right (Recipe " ++ show m ++ " " ++ (case show s of "[[]]" -> "[]"; x -> x) ++ ")\n"


generate :: IO ()
generate = do
    file <- readFile "canonical.yaml"
    case parse testFile "" file of
        Left x -> putStrLn $ errorBundlePretty x
        Right x -> putStrLn $ toHspecFile x
    return ()

toHspecFile :: [Test] -> String
toHspecFile xs = header ++ unlines (map show xs)
    where
        header = unlines
            ["--Canonical Tests for the Cooklang project"
            , "import Test.Hspec"
            , "import Cook\n"
            , "main :: IO ()"
            , "main = hspec $ do"
            , "    describe \"Canonical\" $ do"]

testFile :: Parser [Test]
testFile = do
    string "version: 5" *> space *> string "tests:" *> space
    some $ test <* space

test :: Parser Test
test = do
    string "test"
    name <- some $ noneOf ":"
    char ':' *> space
    string "source: |\n"
    input <- some $ (string "      " *> some (noneOf "\n") <* newline) <|> fmap (:[]) newline
    space *> string "result:"
    space *> string "steps:" *> space
    resultSteps <- some $ step <* space
    resultMetadata <- space *> string "metadata:" *> space *> ((string "{}" *> return []) <|> some (metadata <* space))
    return $ Test name (intercalate "\n" input) resultMetadata resultSteps

step :: Parser Step
step = do
    string "[]" *> return []
    <|> (do
            char '-' *> space
            some $ (try text <|> try ingredient <|> try cookware <|> timer <?> "step part") <* space)

metadata :: Parser Metadata
metadata = do
    space *> char '\"'
    key <- some $ noneOf "\""
    string "\": "
    value <- some $ noneOf "\n"
    return (key, value)

text :: Parser Content
text = do
    string "- type: text" *> space *> string "value: \""
    content <- some $ noneOf "\""
    char '\"'
    return $ Text content

ingredient :: Parser Content
ingredient = do
    string "- type: ingredient" *> space *> string "name: "
    name <- char '\"' *> some (noneOf "\"") <* char '\"' <* space <* string "quantity: "
    amount <- optional (char '"') *> many (noneOf "\n\"") <* optional (char '"') <* space <* string "units: "
    units <- char '\"' *> many (noneOf "\"") <* char '\"' <* space
    return $ Ingredient name amount units

cookware :: Parser Content
cookware = do
    string "- type: cookware" *> space *> string "name: \""
    name <- many $ noneOf "\""
    char '\"' *> space *> string "quantity: "
    amount <- many $ noneOf "\n"
    return $ Cookware name amount

timer :: Parser Content
timer = do
    string "- type: timer" *> space *> string "quantity: "
    amount <- many $ noneOf "\n"
    space *> string "units: \""
    units <- many $ noneOf "\""
    char '\"' *> space *> string "name: \""
    name <- many $ noneOf "\""
    char '\"'
    return $ Timer name amount units
    