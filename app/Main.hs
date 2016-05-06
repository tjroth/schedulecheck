{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Options.Generic
import           Schedule

data CliApp = CliApp { file :: String }
  deriving (Generic, Show)

instance ParseRecord CliApp

main :: IO ()
main = do
  x <- getRecord "Schedule Analyzer"
  csvData <- BL.readFile (file x)
  let sched = scheduleFromByteString csvData
  case sched of
    Left e -> putStrLn e
    Right s -> do
      {--mapM_ print (rotations s)
      putStrLn "\n\n"
      mapM_ print (dates s)
      print (startDate s)
      print (endDate s)
      mapM_ print (entries s)
      --let cnts = M.toList (rotationCounts s)
      --mapM_ print cnts
      --print $ dailySchedule s--}
      printDailySchedule s
      --let sched = dailySchedule s
