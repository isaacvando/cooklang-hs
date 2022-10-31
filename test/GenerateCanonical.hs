import Test.Hspec
import Cook hiding (step, metadata, ingredient, cookware, timer) -- (Metadata, Step, Content)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

-- type Parser = Parsec Void String
        -- Name Input Metadatas Steps
data Test = Test String String [Metadata] [Step] deriving Show

generate :: IO ()
generate = do
    file <- readFile "test/canonical.yaml"
    print $ parse test "" file
    return ()

test :: Parser Test
test = do
    string "test"
    name <- some $ noneOf ":"
    char ':' *> space
    string "source: |\n" *> space
    input <- some $ noneOf "\n"
    space *> string "result:"
    space *> string "steps:" *> space
    resultSteps <- some step
    resultMetadata <- space *> string "metadata:" *> space *> (string "{}" *> return []) <|> some metadata
    return $ Test name input resultMetadata resultSteps

step :: Parser Step
step = do
    (string "[]" *> return []) 
    <|> (do 
            char '-' *> space
            contents <- some text
            return contents)

metadata :: Parser Metadata
metadata = do 
    space *> char '\"'
    key <- some $ noneOf "\""
    string "\":" *> space
    value <- some $ noneOf "\n"
    return (key, value)


text :: Parser Content
text = do 
    string "- type: text" *> space *> string "value: \""
    content <- some $ noneOf "\""
    char '\"'
    return $ Text content