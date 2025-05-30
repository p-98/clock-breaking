{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module Lib
  ( -- * Public API
    solveACM
  , solveDemo
    -- * Exported for tests only
  , printDisplay
  , Time(..)
  , timeTo7Segments
  ) where

import Control.Conditional (select, (??))
import Data.Function ((&))
import Data.List (intercalate, intersect, transpose, union)
import Data.List.Extra (chunksOf)

-- * Entry Points

solveACM :: String -> String
solveACM = printClockDiagnostic . compareAllTimeSeries . parseInput

solveDemo :: String -> String
solveDemo str = intercalate "\n"
  [ "Input:"
  , concatMap (box . printDisplay ("█" ?? " ") " ") input
  , "Possible start times:"
  , ( intercalate ", " . map (show) . possibleStartTimes) input ++ "\n"
  , "Result:"
  , (prettyClockDiagnostic . compareAllTimeSeries) input
  ]
  where
    input = parseInput str
    possibleStartTimes :: [Display Segment] -> [Time]
    possibleStartTimes i
      = filter
        (not
        . any null
        . compareTimeSeries i
        . map timeTo7Segments . iterate nextTime)
      $ allTimes

-- * Business Logic

type Segment = Bool
type Display a = [a]

data SegmentCondition = Working | BurnIn | BurnOut deriving (Show, Eq)
allConditions :: [SegmentCondition]
allConditions = [Working, BurnIn, BurnOut]
type PossibleConditions = [SegmentCondition]

data SegmentDiagnostic = Unknown | Known SegmentCondition deriving (Show)
data ClockDiagnostic = Impossible | ClockDiagnostic (Display SegmentDiagnostic)

compareAllTimeSeries :: [Display Segment] -> ClockDiagnostic
compareAllTimeSeries actual = do
  let onlyPossible
        = filter (not . any null)
        $ map (compareTimeSeries actual)
        $ allTimeSeries
  if null onlyPossible
    then Impossible
    else ClockDiagnostic $ diagnoseClock $ foldr2d union [] onlyPossible

diagnoseClock :: Display PossibleConditions -> Display SegmentDiagnostic
diagnoseClock = map diagnoseSegment
  where
    diagnoseSegment [] = error "Internal error: Impossible segment should have been removed"
    diagnoseSegment [cond] = Known cond
    diagnoseSegment (_ : _ : _) = Unknown

compareTimeSeries
  :: {- actual -} [Display Segment] -> {- expected -} [Display Segment]
  -> Display PossibleConditions
compareTimeSeries
  = foldr2d intersect allConditions
  .: zipWith compareDisplay


-- | The possible conditions of segment within a display given an expected display
compareDisplay
  :: {- actual -} Display Segment -> {- expected -} Display Segment
  -> Display PossibleConditions
compareDisplay = zipWith compareSegment
  where
    compareSegment False False = [BurnOut, Working]
    compareSegment False True  = [BurnOut]
    compareSegment True  False = [BurnIn]
    compareSegment True  True  = [BurnIn, Working]


data Time = Time { hours :: Int, minutes :: Int }
instance Show Time where
  show Time{hours, minutes} = show hours ++ ":" ++ show minutes

allTimeSeries :: [[Display Segment]]
allTimeSeries = map2d timeTo7Segments $ map (iterate nextTime) $ allTimes

allTimes :: [Time]
allTimes = [Time hours minutes | hours <- [0 .. 23], minutes <- [0 .. 59]]

nextTime :: Time -> Time
nextTime (Time 23 59) = Time 00 00
nextTime (Time hs 59) = Time (hs + 1) 00
nextTime (Time hs ms) = Time hs (ms + 1)

timeTo7Segments :: Time -> Display Segment
timeTo7Segments Time{hours, minutes}
  = select (0 ==) (const $ replicate 7 False) digitTo7Segments (digit 1 hours)
  ++ digitTo7Segments (digit 0 hours)
  ++ [True, True]
  ++ digitTo7Segments (digit 1 minutes)
  ++ digitTo7Segments (digit 0 minutes)

digitTo7Segments :: Int -> [Segment]
digitTo7Segments 0 = [True , True , True , True , True , True , False]
digitTo7Segments 1 = [False, True , True , False, False, False, False]
digitTo7Segments 2 = [True , True , False, True , True , False, True ]
digitTo7Segments 3 = [True , True , True , True , False, False, True ]
digitTo7Segments 4 = [False, True , True , False, False, True , True ]
digitTo7Segments 5 = [True , False, True , True , False, True , True ]
digitTo7Segments 6 = [True , False, True , True , True , True , True ]
digitTo7Segments 7 = [True , True , True , False, False, False, False]
digitTo7Segments 8 = [True , True , True , True , True , True , True ]
digitTo7Segments 9 = [True , True , True , True , False, True , True ]
digitTo7Segments d = error $ "Internal error: Expected digit, but got " ++ show d

-- * Parsing

parseInput :: String -> [Display Segment]
parseInput
  = fmap (parseTime . unlines)
  . chunksOf 7
  . filter (not . null)
  . drop 1
  . lines

parseTime :: String -> Display Segment
parseTime
  [_ ,a1,_ ,_ ,_ ,_ ,a2,_ ,_ ,_ ,_   ,_ ,_ ,a3,_ ,_ ,_ ,_ ,a4,_ ,_ ,'\n'
  ,f1,_ ,_ ,b1,_ ,f2,_ ,_ ,b2,_ ,_   ,_ ,f3,_ ,_ ,b3,_ ,f4,_ ,_ ,b4,'\n'
  ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,dot1,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,'\n'
  ,_ ,g1,_ ,_ ,_ ,_ ,g2,_ ,_ ,_ ,_   ,_ ,_ ,g3,_ ,_ ,_ ,_ ,g4,_ ,_ ,'\n'
  ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,dot2,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,'\n'
  ,e1,_ ,_ ,c1,_ ,e2,_ ,_ ,c2,_ ,_   ,_ ,e3,_ ,_ ,c3,_ ,e4,_ ,_ ,c4,'\n'
  ,_ ,d1,_ ,_ ,_ ,_ ,d2,_ ,_ ,_ ,_   ,_ ,_ ,d3,_ ,_ ,_ ,_ ,d4,_ ,_ ,'\n']
  = ('X' ==)
  <$>
    [ a1, b1, c1, d1, e1, f1, g1
    , a2, b2, c2, d2, e2, f2, g2
    , dot1, dot2
    , a3, b3, c3, d3, e3, f3, g3
    , a4, b4, c4, d4, e4, f4, g4 ]
parseTime s = error $ "Expected time, but got:\n" ++ show s

-- * Printing
--
-- Functions in here come in `print...` and `pretty...` variants. The former
-- output in the format specified in the problem definition. The latter is a
-- more human-readable format.

-- | Print clock diagnostic
printClockDiagnostic :: ClockDiagnostic -> String
printClockDiagnostic Impossible = "impossible\n"
printClockDiagnostic (ClockDiagnostic d)
  = printDisplay printSegmentDiagnostic "." d

printSegmentDiagnostic :: SegmentDiagnostic -> String
printSegmentDiagnostic Unknown = "?"
printSegmentDiagnostic (Known Working) = "W"
printSegmentDiagnostic (Known BurnIn) = "1"
printSegmentDiagnostic (Known BurnOut) = "0"

printDisplay :: Show a => (a -> String) -> String -> Display a -> String
printDisplay f __ ds = f <$> ds & \case {
  [ a1, b1, c1, d1, e1, f1, g1, a2, b2, c2, d2, e2, f2, g2, dot1, dot2
  , a3, b3, c3, d3, e3, f3, g3, a4, b4, c4, d4, e4, f4, g4 ]
  -> concat
    [__,a1,a1,__,__,__,a2,a2,__,__,__  ,__,__,a3,a3,__,__,__,a4,a4,__,"\n"
    ,f1,__,__,b1,__,f2,__,__,b2,__,__  ,__,f3,__,__,b3,__,f4,__,__,b4,"\n"
    ,f1,__,__,b1,__,f2,__,__,b2,__,dot1,__,f3,__,__,b3,__,f4,__,__,b4,"\n"
    ,__,g1,g1,__,__,__,g2,g2,__,__,__  ,__,__,g3,g3,__,__,__,g4,g4,__,"\n"
    ,e1,__,__,c1,__,e2,__,__,c2,__,dot2,__,e3,__,__,c3,__,e4,__,__,c4,"\n"
    ,e1,__,__,c1,__,e2,__,__,c2,__,__  ,__,e3,__,__,c3,__,e4,__,__,c4,"\n"
    ,__,d1,d1,__,__,__,d2,d2,__,__,__  ,__,__,d3,d3,__,__,__,d4,d4,__,"\n"];
  _ -> error $ "Internal error: Expected display, but got " ++ show ds
}

prettyClockDiagnostic :: ClockDiagnostic -> String
prettyClockDiagnostic Impossible = "impossible\n"
prettyClockDiagnostic (ClockDiagnostic d)
  = box $ printDisplay prettySegmentDiagnostic " " d

prettySegmentDiagnostic :: SegmentDiagnostic -> String
prettySegmentDiagnostic Unknown = "\x1b[43m?\x1b[m"
prettySegmentDiagnostic (Known Working) = "\x1b[42m✓\x1b[m"
prettySegmentDiagnostic (Known BurnIn) = "\x1b[41m1\x1b[m"
prettySegmentDiagnostic (Known BurnOut) = "\x1b[41m0\x1b[m"

box :: String -> String
box str
  = "╭" ++ replicate width '─' ++ "╮\n"
  ++ mapLines (("│" ++) . (++ "│")) str
  ++ "╰" ++ replicate width '─' ++ "╯\n"
  where
      width = ansiiLength $ takeWhile ('\n' /=) str
      -- | length of a string taking ansii color codes into account
      ansiiLength ('\x1b' : '[' : rest) = ansiiLength $ tail $ dropWhile ('m' /=) rest
      ansiiLength (_ : rest) = 1 + ansiiLength rest
      ansiiLength [] = 0
      mapLines f = unlines . map f . lines

-- * General Purpose Helpers

-- | Nth lowest digit of an integer
digit :: Int -> Int -> Int
digit d n = (n `div` (10 ^ d)) `mod` 10

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map $ map f

-- | fold along the second dimension
foldr2d :: (a -> b -> b) -> b -> [[a]] -> [b]
foldr2d f x = map (foldr f x) . transpose
