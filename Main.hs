module Main (main) where

import Text.Cook ( parseCook )

main :: IO ()
main = do
    input <- getContents
    case parseCook input of
        Left parseError -> putStrLn parseError
        Right result -> print result
