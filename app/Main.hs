{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad        (mzero)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.List            (nub, sort)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Lib
import           Safe
import Data.Time.Format
import Data.Time
import qualified Data.ByteString.Char8 as C8

data Entry = Entry {
  date     :: !UTCTime,
  rotation :: !Text,
  person   :: !Text } deriving (Show, Eq)

data Schedule = Schedule {
  entries :: V.Vector Entry } deriving (Show, Eq)

instance FromRecord Entry where
  parseRecord v
    | V.length v == 11 = Entry <$> v .! 0 <*> v .! 2 <*> v .! 9
    | otherwise = mzero


instance FromField UTCTime where
  parseField t = pure $ parseTimeOrError True defaultTimeLocale "%-m/%-d/%Y" (C8.unpack t)
--  parseField t
--    | t= Other
scheduleFromByteString :: BL.ByteString -> Either String Schedule
scheduleFromByteString bs =
  let es = decode HasHeader bs :: Either String (V.Vector Entry) in
  Schedule <$> es

rotations :: Schedule -> [Text]
rotations sched =
  let sls = V.toList (entries sched) in
  sort . nub $ map rotation sls

people :: Schedule -> [Text]
people sched =
  let sls = V.toList (entries sched) in
  sort . nub $ map person sls

dates :: Schedule -> [UTCTime]
dates sched =
  let sls = V.toList (entries sched) in
  nub $ map date sls

startDate :: Schedule -> Maybe UTCTime
startDate sched = headMay (dates sched)

endDate :: Schedule -> Maybe UTCTime
endDate sched = lastMay (dates sched)

rotationCounts :: Schedule -> M.Map (Text,Text) Integer
rotationCounts sched = countMap
  where
    countMap = foldl updateMap M.empty tps
    updateMap mp k = if M.notMember k mp
      then M.insert k 1 mp
      else M.adjust (+ 1) k mp
    sList = V.toList (entries sched)
    tps = map (\e -> (rotation e, person e)) sList


scheduleFile = "/Users/toddroth/Downloads/schedule.txt"

main :: IO ()
main = do
  csvData <- BL.readFile scheduleFile
  let sched = scheduleFromByteString csvData
  case sched of
    Left e -> putStrLn e
    Right s -> do

      mapM_ print (rotations s)
      putStrLn "\n\n"
      mapM_ print (dates s)
      print (startDate s)
      print (endDate s)
      mapM_ print (entries s)
      let cnts = M.toList (rotationCounts s)
      mapM_ print cnts

{-}
  [RuleSet] -> Schedule -> Analyzer
  for every day in Schedule apply ruleset to day

type RuleSet = [Rule]

data DateType = AnyDay | DaysOfWeek [Integer] | OnDate Date | Step Integer

  data Rule = Rule {
  person :: Maybe String,
  rotation :: Maybe String,
  date :: DateType
  --assertion :: Bool
}
-}
