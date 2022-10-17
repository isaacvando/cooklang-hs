module Main (main) where

import Cook

main :: IO ()
main = do
    putStrLn "input:"
    input <- getContents
    case parseCook input of
        Left parseError -> putStrLn parseError
        Right result -> print result
