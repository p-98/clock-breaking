module Main (main) where

import Lib (solveDemo)

main :: IO ()
main = putStrLn =<< solveDemo <$> getContents
