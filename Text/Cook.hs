-- Copyright Isaac Van Doren 2022-present

module Text.Cook (Result (..), Content (..), Category (..), Item (..), Step, Metadata, parseCook) where

import Control.Monad (void, when)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, errorBundlePretty, many, noneOf, optional, parse, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, hspace, newline, space, string)
import Text.Printf (printf)

type Parser = Parsec Void String

type Metadata = (String, String)

type Step = [Content]

-- | Represents the different types of content that can be appear in a recipe. The strings may be empty.
data Content
  = -- | Plain text
    Text String
  | -- | An ingredient annotation with label, quantity, and unit
    Ingredient String String String
  | -- | A timer annotation with label, quantity, and unit
    Timer String String String
  | -- | A cookware annotation with label and quantity
    Cookware String String
  deriving (Show, Eq)

-- | An item on a shopping list
data Item = Item String String
  deriving (Show, Eq)

-- | A section on a shopping list
data Category = Category String [Item]
  deriving (Show, Eq)

-- | The result of parsing a .cook file
data Result
  = -- | A recipe
    Recipe [Metadata] [Step]
  | -- | A shopping list
    Grouping [Category]
  deriving (Show, Eq)

-- | Parse a .cook file
parseCook :: String -> Either String Result
parseCook input = case parse cookFile "" (simplify input) of
  Left bundle -> Left $ errorBundlePretty bundle
  Right result -> Right result

simplify :: String -> String
simplify = inlineComments . blockComments
  where
    inlineComments = unlines . map f . lines
      where
        f ('-' : '-' : _) = ""
        f (x : xs) = x : f xs
        f "" = ""
    blockComments :: String -> String
    blockComments ('[' : '-' : xs) = blockComments (consume xs)
      where
        consume ('-' : ']' : ys) = ys
        consume (_ : ys) = consume ys
        consume "" = ""
    blockComments (x : xs) = x : blockComments xs
    blockComments "" = ""

cookFile :: Parser Result
cookFile = grouping <|> recipe

recipe :: Parser Result
recipe = do
  pieces <- space *> many (metadata <* space <|> step <* space)
  return $ foldr go (Recipe [] []) pieces
  where
    go (Recipe m s) (Recipe m' s') = Recipe (m ++ m') (s ++ s')
    go x y = error $ "Not expecting " ++ show x ++ " and " ++ show y ++ " in recipe parser."

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
ingredient =
  char '@'
    *> hspace
    *> ( try
           ( do
               content <- some $ noneOf "#~@\n{"
               void $ char '{'
               (amount, units) <- quantity
               void $ char '}'
               return $ Ingredient (norm content) (case amount of "" -> "some"; x -> x) units
           )
           <|> fmap (\x -> Ingredient x "some" "") word
       )

cookware :: Parser Content
cookware =
  char '#'
    *> hspace
    *> ( try
           ( do
               content <- some $ noneOf "#~@\n{"
               (amount, _) <- char '{' *> quantity <* char '}'
               return $ Cookware (norm content) (case amount of "" -> "1"; x -> x)
           )
           <|> fmap (\x -> Cookware x "1") word
       )

timer :: Parser Content
timer =
  char '~'
    *> hspace
    *> ( try
           ( do
               timerLabel <- many $ noneOf "#~@\n{"
               void $ char '{'
               (amount, units) <- quantity
               void $ char '}'
               return $ Timer (norm timerLabel) amount units
           )
           <|> fmap (\x -> Timer x "" "") word
       )

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
  return $
    let x = printf "%.2f" ((read n1 / read n2) :: Double)
     in if last x == '0' then init x else x

word :: Parser String
word = some $ noneOf " \t\n"

-- removes extra whitespace
norm :: String -> String
norm = unwords . words
