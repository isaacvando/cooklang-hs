module Main (main) where

import Lib

main :: IO ()
main = do
    input <- getContents
    case parseCook input of
        Left parseError -> putStrLn parseError
        Right result -> putStrLn result
