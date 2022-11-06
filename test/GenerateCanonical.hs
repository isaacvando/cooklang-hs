import Test.Hspec
import Cook hiding (step, metadata, ingredient, cookware, timer) -- (Metadata, Step, Content)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
-- import Data.Yaml hiding (Parser) -- TODO: use Data.Yaml instead

-- type Parser = Parsec Void String
        -- Name Input Metadatas Steps
data Test = Test String String [Metadata] [Step] deriving Show

-- generate' :: IO ()
-- generate' = do
--     val <- decodeFileEither "test/canonical.yaml"
--     print val

generate :: IO ()
generate = do
    file <- readFile "test/canonical.yaml"
    case (parse testFile "" file) of
        Left x -> putStrLn $ errorBundlePretty x
        Right x -> print x
    return ()

testFile :: Parser [Test]
testFile = do
    string "version: 5" *> space *> string "tests:" *> space
    ts <- some $ test <* space
    return ts

test :: Parser Test
test = do
    string "test"
    name <- some $ noneOf ":"
    char ':' *> space
    string "source: |\n"
    input <- some $ (string "      " *> (some $ noneOf "\n") <* newline) <|> fmap (:[]) newline
    space *> string "result:"
    space *> string "steps:" *> space
    resultSteps <- some $ step <* space
    resultMetadata <- space *> string "metadata:" *> space *> ((string "{}" *> return []) <|> some (metadata <* space))
    return $ Test name (intercalate "\n" input) resultMetadata resultSteps

step :: Parser Step
step = do
    (string "[]" *> return []) 
    <|> (do 
            char '-' *> space
            contents <- some $ (try text <|> try ingredient <|> try cookware <|> timer <?> "step part") <* space
            return contents)

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
    name <- char '\"' *> (some $ noneOf "\"") <* char '\"' <* space <* string "quantity: "
    amount <- (many $ noneOf "\n") <* space <* string "units: "
    units <- char '\"' *> (many $ noneOf "\"") <* char '\"' <* space
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
