{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main(main) where

import Control.DeepSeq (deepseq)
import Control.Monad (replicateM)
import Criterion (bench, bgroup, nf)
import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types (timeLimit)
import GHC.Utils.Misc (unzipWith)
import Lib (allTimes, compareAllTimeSeries, Display, nextTime, Segment, SegmentCondition(..), Time, timeTo7Segments)
import System.Random.Stateful (mkStdGen, runStateGen_, StatefulGen, uniformM, uniformRM)

clocks :: Int
clocks = 25

seed :: Int
seed = 42

type Clock = Display SegmentCondition

main :: IO ()
main = defaultMainWith (defaultConfig {timeLimit = 5})
  -- [ bgroup "solveACM" $ benchSolveACM <$> [1, 10, 50, 100] ++ [144, 288 .. 1440]
  [ bgroup "solveACM" $ benchSolveACM <$> [1..9] ++ [10, 15 .. 45] ++ [50, 60 .. 150]
  ]
  where
    benchSolveACM n = do
      let inputs = genInputs n
      inputs `deepseq` (bench (show n) $ nf (compareAllTimeSeries <$>) inputs)

genInputs :: Int -> [[Display Segment]]
genInputs n
  = runStateGen_ (mkStdGen seed) $ \g
  -> unzipWith (\c t -> map (display c) $ take n $ iterate nextTime t)
  <$> replicateM clocks ((,) <$> randomClock g <*> randomTime g)

display :: Clock -> Time -> Display Segment
display clock = timeTo7Segments .> flip zipWith clock \cases
  Working s -> s
  BurnIn _ -> True
  BurnOut _ -> False

randomClock :: StatefulGen g m => g -> m Clock
randomClock g = replicateM (4 * 7 + 2) $ uniformM g

randomTime :: StatefulGen g m => g -> m Time
randomTime = randomElement -$ allTimes

randomElement :: StatefulGen g m => g -> [a] -> m a
randomElement g xs = (xs !!) <$> uniformRM (0, length xs - 1) g

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(-$) :: (a -> b -> c) -> b -> a -> c
(-$) f y x = f x y
