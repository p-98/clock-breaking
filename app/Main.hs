module Main (main) where

import Lib (solveACM)

main :: IO ()
main = putStrLn =<< solveACM <$> getContents
