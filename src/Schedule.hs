{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Schedule where

import           Control.Monad         (mzero)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv
import           Data.List             (nub, nubBy, sort, last, foldl', groupBy, (\\))
import qualified Data.HashMap.Lazy       as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time
import           Data.Time.Format
import qualified Data.Vector           as V
import           Lib
import           Safe
import           Data.Maybe
import qualified       Data.Set          as S

data Schedule = Schedule {
  events :: [Event] } deriving (Show, Eq)

data Task = Task {
  name :: !Text,
  days :: [DayOfWeek],
  pool :: [Text],
  count :: Int } deriving (Read, Show, Eq)

data Event = Event {
  date     :: !Day,
  rotation :: !Text,
  person   :: Maybe Text } deriving (Show, Eq)

data Period = Period {
  start :: Day,
  end :: Day} deriving (Read, Show, Eq)


{-
instance FromRecord Event where
  parseRecord v
    | V.length v == 11 = Event <$> v .! 0 <*> v .! 2 <*> v .! 9
    | otherwise = mzero


instance FromField UTCTime where
  parseField t = pure $ parseTimeOrError True defaultTimeLocale "%-m/%-d/%Y" (C8.unpack t)

scheduleFromByteString :: BL.ByteString -> Either String Schedule
scheduleFromByteString bs =
  let es = decode HasHeader bs :: Either String (V.Vector Event) in
    Schedule <$> es
-}
csvFromQgendaGrid :: BL.ByteString -> Either String (V.Vector [T.Text])
csvFromQgendaGrid bs = decode NoHeader bs

removeEmptyText = filter (\x -> (x /= ""))

csvToEvents :: V.Vector [T.Text] -> [Event] 
csvToEvents csv = cleanNames $ filter (\ev -> (person ev) /= Just "") evs
  where
    cleanCsv = V.drop 3 csv -- remove the header lines
    entries = V.toList $ V.drop 3 cleanCsv
    myRow = removeEmptyText $ cleanCsv V.! 0
    dayRow = removeEmptyText $ cleanCsv V.! 1    
    dtStr = T.unpack $ T.take 3 (head myRow) <> head dayRow <> (T.drop 4 $ head myRow) 
    startDt = parseTimeOrError True defaultTimeLocale "%b%e%y" dtStr :: Day 
    updateEntry acc [] = acc
    updateEntry acc@(f:fs) e = if (head e) == ""
                             then
                               let newRow = (head f ) : (drop 1 e) in
                                 newRow : acc
                             else e : acc
    updatedEntries = foldl' updateEntry [(head entries)] (drop 1 entries)
    evs = concatMap (\e -> zipWith3 Event [startDt ..] (repeat $ head e) (map Just (drop 1 e))) updatedEntries
    cleanNames = map (\e@Event{..} -> e { person = (T.take 3) <$> person})

{-
makeTaskDef :: [Event] -> [(Text, [DayOfWeek], [Text])]
makeTaskDef = collect . partition
  where
    partition = groupBy (\a b -> (rotation a == rotation b))
    collect = map (\gp -> ((rotation . head $ gp),
                           (nub (map (dayOfWeek . date) gp)),
                           sort (nub (map person gp))))
-}

subSchedule :: Schedule -> [Day] -> [Text] -> Schedule
subSchedule sched ds tns = sched {events = newEvs}
  where
    newEvs = filter (\ev -> ((date ev) `elem` ds) && ((rotation ev) `elem` tns)) (events sched)

makeTaskDef' :: [Event] -> [Task] --[(Text, [DayOfWeek], [Text])]
makeTaskDef' = collect . partition
  where
    partition = groupBy (\a b -> (rotation a == rotation b))
    collect = map (\gp -> Task ((rotation . head $ gp))
                          (nub (map (dayOfWeek . date) gp))
                          (sort . nub . catMaybes $ map person gp)
                  1 )

taskToEvents :: Task -> [Day] -> [Event]
taskToEvents tsk dst = map (\d -> Event d (name tsk) Nothing) ds
  where
    ds = filter (\d -> (dayOfWeek d) `elem` (days tsk))  dst --[(start p) .. (end p)]


validateSchedule :: Schedule -> [Day] -> [Task]  -> [Text]
validateSchedule sched ds ts = concatMap (validateTask sched ds) ts <>
                               concatMap (validateAssignments sched ds) ts

validateTask :: Schedule -> [Day] -> Task -> [Text]  
validateTask sched ds tsk =  notPresent ++ extra
  where
    notPresent = map (\d -> "Task Error: " <> (name tsk) <> " not assigned on "  <> T.pack (show (dayOfWeek d)) <> " " <> T.pack (show d)) (S.toList ((S.\\) tgtDts actualDts))
    extra = map (\d -> "Task Error: " <> (name tsk) <> " should not be assigned on "  <> T.pack (show (dayOfWeek d)) <> " " <> T.pack (show d)) (S.toList ((S.\\)actualDts  tgtDts))
    evs = (events sched)
    tgtDts = S.fromList $ map (\e -> date e) $ taskToEvents tsk ds
    actualDts = S.fromList $ map (\e -> date e) $ filter (\e -> (rotation e) == (name tsk)) evs

validateAssignments :: Schedule -> [Day] -> Task -> [Text]
validateAssignments sched ds tsk = map (T.pack . show) $ filter validAssign evs
  where
    evs = events $ subSchedule sched ds [(name tsk)]
    validAssign e = case (person e) of
      Just p -> p `notElem` (pool tsk)
      Nothing -> True

tgtTsk = Task {name = "BODY-CED", days = [Tuesday,Friday], pool = ["BOR","EDW","JGA","KDP","MOR","TJR","WAT"], Schedule.count = 1}

run = do
  qFile <- BL.readFile "/Users/toddroth/Desktop/qgenda2020-07-01thru2020-12-30.csv"
  let csvData = csvFromQgendaGrid qFile
  case csvData of
    Left s -> return [] 
    Right v -> return $ csvToEvents v


validateIO = do
  qFile <- BL.readFile "/Users/toddroth/Desktop/qgenda2020-07-01thru2020-12-30.csv"
  let csvData = csvFromQgendaGrid qFile
  let tgtTaskNms = ["BODY-WMR", "BODY-CED", "BODY-WFO", "Body Wknd", "Neuro Wknd"]
  let start = fromGregorian 2020 6 29
  let end = fromGregorian 2020 12 22
  let period = Period start end
  let exclude = [(fromGregorian 2020 7 3), (fromGregorian 2020 9 7), (fromGregorian 2020 11 26), (fromGregorian 2020 11 27)]
  let ds = filter (\x -> x `notElem` exclude) [start .. end]
  case csvData of
    Left s -> print "Error"
    Right v -> mapM_ print $ validateSchedule (subSchedule (Schedule (csvToEvents v)) ds tgtTaskNms) ds (filter (\tsk -> (name tsk) `elem` tgtTaskNms ) tasks) 


rotations :: Schedule -> [Text]
rotations sched =
  let sls = (events sched) in
    sort . nub $ map rotation sls

people :: Schedule -> [Text]
people sched =
  let sls = (events sched) in
  sort . nub . catMaybes $ map person sls

dates :: Schedule -> [Day]
dates sched =
  let sls = (events sched) in
  sort . nub $ map date sls

startDate :: Schedule -> Maybe Day --UTCTime
startDate sched = headMay (dates sched)

endDate :: Schedule -> Maybe Day --UTCTime
endDate sched = lastMay (dates sched)
{-
rotationCounts :: Schedule -> M.HashMap (Text,Text) Integer
rotationCounts sched = countMap
  where
    countMap = foldl updateMap M.empty tps
    updateMap mp k = if (not $ M.member k mp)
      then M.insert k 1 mp
      else M.adjust (+ 1) k mp
    sList = V.toList (entries sched)
    tps = map (\e -> (rotation e, person e)) sList

dailySchedule :: Schedule -> M.HashMap UTCTime [(Text, Text)]
dailySchedule sched = sMap
  where
    sMap = foldl updateMap M.empty (V.toList (entries sched))
    updateMap mp (Entry d r p) = M.insertWith (++) d [(r,p)] mp

printDailySchedule ::  Schedule -> IO ()
printDailySchedule sched = mapM_ printDay ds
  where
    printDay t = do
      print $ fst t
      mapM_ (\e -> putStrLn ("\t" ++ T.unpack (fst e) ++ "   " ++ T.unpack (snd e))) (sort $ snd t)
    ds = M.toList (dailySchedule sched)
-}
scheduleFile = "/Users/toddroth/Downloads/schedule.txt"


{-
  [RuleSet] -> Schedule -> Analyzer
  for every day in Schedule apply ruleset to day

type RuleSet = [Rule]

data DateType = AnyDay | DaysOfWeek [Integer] | OnDate Date | Step Integer

  data Rule = Rule {
  person :: Maybe String,
  rotation :: Maybe String,
  date :: DateType
  --assertion :: Bool

-}


tasks = [Task {name = "TO", days = [Friday,Wednesday,Tuesday,Thursday,Monday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"], count = 1},
         Task {name = "Non Neuro", days = [Saturday], pool = ["ADM","BOR","BRD","BWN","CML","CRO","DAL","DGD","DOS","HAR","HIN","JKT","JLB","KDP","KEE","LOT","PEN","RBC","SHK","SOP","WAT"], count = 1},
         Task {name = "PA WMRam/BRpm", days = [Monday,Thursday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA WMRam/CLYpm", days = [Tuesday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA WMR", days = [Thursday,Wednesday,Monday,Tuesday,Friday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA BRam/CLYpm", days = [Tuesday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA CDH", days = [Wednesday,Friday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA BRO pm", days = [Monday], pool = ["KDM"], count = 1},
         Task {name = "PA BRO", days = [Thursday,Monday], pool = ["KDM","RRB"], count = 1},
         Task {name = "PA Vasc Call", days = [Saturday,Sunday], pool = ["KDM"], count = 1},
         Task {name = "NoC", days = [Saturday,Sunday], pool = ["ADM","BRD","BWN","DGD","DOS","HAR","JLB","KEE","RED"], count = 1},
         Task {name = "FLEX", days = [Friday], pool = ["ADM"], count = 1},
         Task {name = "Peds Overnight Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["JLB","LKB","LLA","MCP"], count = 1},
         Task {name = "NeuroSurg Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["KLA"], count = 1},
         Task {name = "Neuro Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["ALT","BWT","HOL","JGA","KHA","KLA","NAZ","RED","TAN"], count = 1},
         Task {name = "MSK Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["BRD","BWN","CAR","HIN","JBM","PEN"], count = 1},
         Task {name = "Body Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["BOR","EDW","JGA","KDP","LOT","MOR","TJR","WAT"], count = 1},
         Task {name = "Vasc Call", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["ADM","CML","DAL","DOS","HAR","KEE","SOP"], count = 1},
         Task {name = "REX Breast Conf", days = [Tuesday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},
         Task {name = "Peds Conf", days = [Thursday,Wednesday], pool = ["JLB","LKB","LLA","MCP"], count = 1},
         Task {name = "Ortho MSK Conf", days = [Friday], pool = ["BRD","BWN","CAR","JBM","PEN","SHA"], count = 1},
         Task {name = "Hand & Wrist Conf", days = [Monday], pool = ["BWN","CAR","PEN","SHA"], count = 1},
         Task {name = "Cary Thoracic Conf", days = [Tuesday], pool = ["BOR","EDW","KDP","MOR","TJR","WAT"], count = 1},
         Task {name = "WMC Breast Conf", days = [Wednesday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},
         Task {name = "Lung nodule conf", days = [Friday], pool = ["BOR","EDW","JGA","KDP","MOR","TJR","WAT"], count = 1},
         Task {name = "Vas Wknd", days = [Saturday,Sunday], pool = ["ADM","CML","DAL","DOS","HAR","KEE","SOP"], count = 1},
         Task {name = "Wknd Gen2", days = [Saturday,Sunday], pool = ["ALT","BOR","BRD","BWT","CML","CRO","DAL","DGD","HAR","HIN","JGA","JKT","KDP","KHA","PEN","SHK","TAN","WAT"], count = 1},
         Task {name = "Wknd Gen1", days = [Saturday,Sunday], pool = ["ADM","BRD","BWN","BWT","CAR","CML","CRO","DGD","DOS","HAR","JKT","JLB","KDP","KEE","LLA","PEN","RED","SHK","SM ","SOP","TAN"], count = 1},
         Task {name = "Peds Wknd", days = [Saturday,Sunday], pool = ["JLB","LKB","LLA","MCP"], count = 1},
         Task {name = "Neuro Wknd", days = [Saturday,Sunday], pool = ["BWT","HOL","JGA","KHA","KLA","RED","TAN"], count = 1},
         Task {name = "MSK Wknd", days = [Saturday,Sunday], pool = ["BRD","BWN","CAR","HIN","JBM","PEN","SHA"], count = 1},
         Task {name = "Body Wknd", days = [Saturday, Sunday], pool = ["BOR","EDW","JGA","KDP","LOT","MOR","TJR","WAT"], count = 1},
         Task {name = "RSNA", days = [Monday,Tuesday,Wednesday], pool = ["BRD","HAR","JLB","SM"], count = 1},
         Task {name = "TRN", days = [Wednesday,Thursday,Monday,Tuesday,Friday], pool = ["ADM","KDM","KEE","PEN","RRB"], count = 1},
         Task {name = "HOff", days = [Monday,Thursday,Friday], pool = ["ADM","BOR","BWN","BWT","CAR","CLO","CML","CRO","DAL","DGD","DOS","EDW","HIN","HOL","JBM","JGA","JKT","KDM","KDP","KEE","KHA","LKB","LLA","LOT","MCP","MOR","PEN","RED","RRB","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"], count = 1},
         Task {name = "PCO", days = [Monday,Wednesday,Thursday,Friday,Tuesday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KHA","LKB","LLA","MCP","MOR","PEN","RED","SHK","SM","SOP","TAN","TJR","WAT"], count = 1},
         Task {name = "LEV", days = [Monday,Wednesday,Friday,Thursday], pool = ["EDW"], count = 1},
         Task {name = "Admin", days = [Wednesday,Thursday,Friday,Tuesday,Monday], pool = ["BWN","HAR","HOL","KDP","LOT","SM","TJR"], count = 1},
         Task {name = "Vac", days = [Wednesday,Thursday,Friday,Tuesday,Monday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDM","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","RRB","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"], count = 1},
         Task {name = "PCO2", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["BOR","BRD","BWN","HOL","JKT","JLB","KDP","KHA","KLA","LLA","MOR","TAN"], count = 1},
         Task {name = "BKU", days = [Thursday,Monday,Tuesday,Wednesday,Friday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","SHK","SM","SOP","TAN","TJR","WAT"], count = 1},
         Task {name = "BDO", days = [Wednesday,Friday,Thursday,Tuesday,Monday], pool = ["LOT"], count = 1},
         Task {name = "DOff", days = [Friday,Tuesday,Monday,Thursday,Wednesday], pool = ["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","NAZ","PEN","RBC","RED","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"], count = 1},
         Task {name = "WM Cardiac MR", days = [Wednesday,Thursday,Friday,Monday,Tuesday], pool = ["EDW","JLB","LLA","TAN","TJR"], count = 1},
         Task {name = "WMR Late", days = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday], pool = ["ALT","NAZ","RBC"], count = 1},
         Task {name = "WMR Eve2", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KHA","LKB","LLA","MCP","MOR","NAZ","PEN","RBC","RED","SHK","SOP","TAN","TJR","WAT"], count = 1},
         Task {name = "WMR Eve1", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RBC","RED","SHK","SM","SOP","TAN","TJR","WAT"], count = 1},Task {name = "HAW", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["BOR","BWN","HAR","HOL","JKT","TAN"], count = 1},Task {name = "VIR-WMC", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","CML","DAL","DOS","HAR","KEE","SM","SOP"], count = 1},Task {name = "VIR-VEIN", days = [Wednesday,Thursday,Monday,Tuesday,Friday], pool = ["ADM","CAR","CLO","CML","DAL","DOS","HAR","KEE","SM","SOP","WAT"], count = 1},Task {name = "VIR2-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","BWN","CML","DAL","DOS","HAR","KEE","KLA","SM","SOP"], count = 1},Task {name = "VIR1-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","CML","DAL","DOS","HAR","KEE","SOP"], count = 1},Task {name = "PEDS-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["JLB","LKB","LLA","MCP"], count = 1},Task {name = "PEDS-RRC/WMList", days = [Tuesday,Wednesday,Thursday,Friday,Monday], pool = ["JLB","LKB","LLA","MCP"], count = 1},Task {name = "NEURO-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ALT","BWT","HOL","JGA","KHA","KLA","NAZ","RED","TAN"], count = 1},Task {name = "NEURO-FV", days = [Monday,Wednesday,Thursday], pool = ["BWT","HOL","JGA","KHA","RED","TAN"], count = 1},Task {name = "NEURO-CLAY", days = [Tuesday,Friday,Monday], pool = ["BWN","BWT","HOL","JGA","KHA","RED","TAN"], count = 1},Task {name = "NEURO-BRO-EVE", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ALT","BWT","HOL","JGA","KHA","KLA","NAZ","RED","TAN"], count = 1},Task {name = "NEURO-KNI", days = [Monday], pool = ["BWT","CLO","HOL","JGA","KHA","RED","TAN"], count = 1},Task {name = "MSK-WMN", days = [Tuesday,Thursday], pool = ["BRD","BWN","CAR","JBM","PEN","SHA"], count = 1},Task {name = "MSK-MRI-CLA", days = [Monday,Tuesday], pool = ["BRD","BWN","CAR","CLO","HIN","HOL","JBM","PEN","SHA"], count = 1},Task {name = "MSK-RRC", days = [Tuesday,Monday,Wednesday,Thursday,Friday], pool = ["BRD","BWN","CAR","HIN","JBM","PEN"], count = 1},Task {name = "MSK-RMP", days = [Monday,Wednesday,Friday], pool = ["BRD","BWN","CAR","EDW","HIN","JBM","MOR","PEN","SHA"], count = 1},Task {name = "MSK-KNI", days = [Tuesday,Thursday,Friday,Monday], pool = ["BRD","BWN","CAR","JBM","PEN","SHA"], count = 1},Task {name = "MSK-BRI", days = [Monday,Wednesday,Friday,Tuesday,Thursday], pool = ["BRD","BWN","CAR","HIN","JBM","PEN","SHA"], count = 1},Task {name = "MAMMLT/GEN-WMN", days = [Friday], pool = ["EDW"], count = 1},Task {name = "MAMMLT/GEN-WF", days = [Tuesday], pool = ["BOR","BWT","CAR","DGD","EDW","JBM","KDP","LLA","SHK","TJR","WAT"], count = 1},Task {name = "MAMMLT/GEN-RMP", days = [Tuesday,Thursday], pool = ["BOR","BRD","BWN","BWT","CAR","CRO","DGD","EDW","JBM","JGA","KDP","KHA","RED","SHA","SHK","TAN","TJR","WAT"], count = 1},Task {name = "MAMMLT/GEN-KNI", days = [Monday,Friday,Wednesday,Thursday,Tuesday], pool = ["BOR","BRD","BWN","BWT","CAR","DGD","EDW","HIN","HOL","JBM","JGA","KDP","KHA","LLA","PEN","SHA","SHK","TAN","TJR","WAT"], count = 1},Task {name = "MAMMLT/GEN-CED", days = [Monday,Wednesday,Thursday], pool = ["BOR","BRD","BWN","BWT","CAR","CRO","DGD","EDW","JBM","JKT","KDP","KHA","LLA","LOT","PEN","RED","SHK","TAN","TJR","WAT"], count = 1},Task {name = "MAMMLT/GEN-BRO", days = [Monday,Wednesday,Thursday,Friday], pool = ["BOR","CRO","DGD","EDW","JGA","JKT","KDP","LOT","SHK","SSL","TJR"], count = 1},Task {name = "GEN-WMApex", days = [Wednesday,Monday,Thursday], pool = ["ADM","BOR","BRD","CAR","DAL","DGD","DOS","EDW","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","MCP","MOR","PEN","RED","SHA","SHK","SM","TAN","TJR","WAT"], count = 1},Task {name = "GEN-WMC", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","SHA","SHK","SM","SOP","TAN","TJR","WAT"], count = 1},Task {name = "GEN-FLUORO-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","MCP","MOR","PEN","SHA","SHK","SM","TAN","TJR","WAT"], count = 1},Task {name = "BREAST-WMN", days = [Friday,Monday,Wednesday], pool = ["CLO","CRO","JKT","LOT","SHK","SSL","TJR"], count = 1},Task {name = "BREAST-WMC/RRC", days = [Monday,Thursday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},Task {name = "BREAST-WMC/BRO", days = [Wednesday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},Task {name = "BREAST-BRO/RRC", days = [Friday,Tuesday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},
         Task {name = "BREAST-BRO", days = [Tuesday], pool = ["CRO","JKT","LOT","SHK","SSL"], count = 1},
         Task {name = "BODY-WMR", days = [Monday,Tuesday,Wednesday,Thursday,Friday], pool = ["BOR","EDW","JGA","KDP","MOR","TJR","WAT"], count = 1},
         Task {name = "BODY-WFO", days = [Monday,Thursday,Wednesday], pool = ["BOR","EDW","JGA","KDP","TJR","WAT"], count = 1},
         Task {name = "BODY-KNI", days = [Wednesday,Friday], pool = ["BOR","EDW","JGA","KDP","WAT"], count = 1},
         Task {name = "BODY-CED", days = [Tuesday,Friday], pool = ["BOR","EDW","JGA","KDP","MOR","TJR", "WAT"], count = 1}]
