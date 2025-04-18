{-# LANGUAGE LambdaCase #-}
import Data.Functor ((<&>))
import Data.List (intercalate)
import Lib (printDisplay, solveACM, Time(..), timeTo7Segments)
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ test
  [ "solveACM" ~:
    ["Official ACM Samples" ~:
      [ sample1
      , sample2
      ]
    ]
  , digitTo7SegmentsAndPrintDisplay
  ]

sample1 :: Test
sample1 = "Sample 1" ~: output ~=? solveACM input
  where
    input =
      "3\n\
      \......XX.....XX...XX.\n\
      \.....X..X...X..X....X\n\
      \.....X..X.X.X..X....X\n\
      \.............XX...XX.\n\
      \.....X..X......X.X..X\n\
      \.....X..X......X.X..X\n\
      \......XX.....XX...XX.\n\
      \\n\
      \......XX.....XX...XX.\n\
      \.....X..X...X..X....X\n\
      \.....X..X.X.X..X....X\n\
      \.............XX...XX.\n\
      \.....X..X......X.X..X\n\
      \.....X..X......X.X..X\n\
      \......XX.....XX...XX.\n\
      \\n\
      \.............XX...XX.\n\
      \........X...X..X....X\n\
      \........X.X.X..X....X\n\
      \.............XX......\n\
      \........X...X..X.X..X\n\
      \........X...X..X.X..X\n\
      \......XX.....XX...XX."
    output =
      ".??...WW.....??...??.\n\
      \?..?.W..?...?..1.0..?\n\
      \?..?.W..?.?.?..1.0..?\n\
      \.??...??.....11...WW.\n\
      \?..?.W..?.0.W..?.1..?\n\
      \?..?.W..?...W..?.1..?\n\
      \.??...11.....??...??."

sample2 :: Test
sample2 = "Sample 2" ~: output ~=? solveACM input
  where
    input =
      "2\n\
      \......XX.....XX...XX.\n\
      \...X....X...X..X.X..X\n\
      \...X....X.X.X..X.X..X\n\
      \......XX..........XX.\n\
      \...X.X....X.X..X.X..X\n\
      \...X.X......X..X.X..X\n\
      \......XX.....XX...XX.\n\
      \\n\
      \......XX.....XX......\n\
      \...X....X...X..X.....\n\
      \...X....X.X.X..X.....\n\
      \......XX.............\n\
      \...X.X....X.X..X.....\n\
      \...X.X......X..X.....\n\
      \......XX.....XX......"
    output =
      "impossible"

digitTo7SegmentsAndPrintDisplay :: Test
digitTo7SegmentsAndPrintDisplay
  = "digitTo7Segments and printDisplay" ~: [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]
  where
    d0 = "Digit 0" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 0) ~?=
      "......XX.....XX...XX.\n\
      \.....X..X...X..X.X..X\n\
      \.....X..X.X.X..X.X..X\n\
      \.....................\n\
      \.....X..X.X.X..X.X..X\n\
      \.....X..X...X..X.X..X\n\
      \......XX.....XX...XX."
    d1 = "Digit 1" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 1) ~?=
      ".....................\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.....................\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \....................."
    d2 = "Digit 2" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 2) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.XX...XX.....XX...XX.\n\
      \X....X....X.X....X...\n\
      \X....X......X....X...\n\
      \.XX...XX.....XX...XX."
    d3 = "Digit 3" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 3) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX."
    d4 = "Digit 4" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 4) ~?=
      ".....................\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \....................."
    d5 = "Digit 5" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 5) ~?=
      ".XX...XX.....XX...XX.\n\
      \X....X......X....X...\n\
      \X....X....X.X....X...\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX."
    d6 = "Digit 6" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 6) ~?=
      ".XX...XX.....XX...XX.\n\
      \X....X......X....X...\n\
      \X....X....X.X....X...\n\
      \.XX...XX.....XX...XX.\n\
      \X..X.X..X.X.X..X.X..X\n\
      \X..X.X..X...X..X.X..X\n\
      \.XX...XX.....XX...XX."
    d7 = "Digit 7" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 7) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.....................\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \....................."
    d8 = "Digit 8" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 8) ~?=
      ".XX...XX.....XX...XX.\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \X..X.X..X.X.X..X.X..X\n\
      \X..X.X..X...X..X.X..X\n\
      \.XX...XX.....XX...XX."
    d9 = "Digit 9" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 9) ~?=
      ".XX...XX.....XX...XX.\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX."
    xx_xx = (\d -> Time (d*10+d) (d*10+d))
    printBool True = 'X'
    printBool False = '.'
