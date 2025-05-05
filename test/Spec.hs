{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub)
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Lib (printDisplay, solveACM, Time(..), timeTo7Segments)
import System.Directory (getDirectoryContents)
import System.FilePath ((<.>), (</>), dropExtension)
import Test.HUnit ((~:), (~=?), (~?=), runTestTTAndExit, test, Test)

main :: IO ()
main = do
  official <- officialInputOutputData
  runTestTTAndExit $ test
    [ "solveACM" ~: official
    , digitTo7SegmentsAndPrintDisplay
    ]

officialInputOutputData :: IO Test
officialInputOutputData = do
  names <- nub . map dropExtension . filter notHidden <$> getDirectoryContents dir
  ios
    <- for names
    $ \n -> (n,,) <$> readFile (dir </> n <.> "in") <*> readFile (dir </> n <.> "ans")
  return $ "Official input/output data" ~: ios <&> uncurry3 mkTest
  where
    dir = "test/official-data/"
    notHidden = not . isPrefixOf "."
    mkTest name input output = name ~: output ~=? solveACM input

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
      \......XX.....XX...XX.\n"
    d1 = "Digit 1" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 1) ~?=
      ".....................\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.....................\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.....................\n"
    d2 = "Digit 2" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 2) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.XX...XX.....XX...XX.\n\
      \X....X....X.X....X...\n\
      \X....X......X....X...\n\
      \.XX...XX.....XX...XX.\n"
    d3 = "Digit 3" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 3) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX.\n"
    d4 = "Digit 4" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 4) ~?=
      ".....................\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.....................\n"
    d5 = "Digit 5" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 5) ~?=
      ".XX...XX.....XX...XX.\n\
      \X....X......X....X...\n\
      \X....X....X.X....X...\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX.\n"
    d6 = "Digit 6" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 6) ~?=
      ".XX...XX.....XX...XX.\n\
      \X....X......X....X...\n\
      \X....X....X.X....X...\n\
      \.XX...XX.....XX...XX.\n\
      \X..X.X..X.X.X..X.X..X\n\
      \X..X.X..X...X..X.X..X\n\
      \.XX...XX.....XX...XX.\n"
    d7 = "Digit 7" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 7) ~?=
      ".XX...XX.....XX...XX.\n\
      \...X....X......X....X\n\
      \...X....X.X....X....X\n\
      \.....................\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.....................\n"
    d8 = "Digit 8" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 8) ~?=
      ".XX...XX.....XX...XX.\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \X..X.X..X.X.X..X.X..X\n\
      \X..X.X..X...X..X.X..X\n\
      \.XX...XX.....XX...XX.\n"
    d9 = "Digit 9" ~: (printDisplay printBool $ timeTo7Segments $ xx_xx 9) ~?=
      ".XX...XX.....XX...XX.\n\
      \X..X.X..X...X..X.X..X\n\
      \X..X.X..X.X.X..X.X..X\n\
      \.XX...XX.....XX...XX.\n\
      \...X....X.X....X....X\n\
      \...X....X......X....X\n\
      \.XX...XX.....XX...XX.\n"
    xx_xx = (\d -> Time (d*10+d) (d*10+d))
    printBool True = 'X'
    printBool False = '.'
