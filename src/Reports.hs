module Reports (
    allReports,
    runTimeOfDayReport,
    runSizeReport,
    runCategoryReport
) where

import Core
import Control.Monad.Reader (MonadReader, ask)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime, localTimeOfDay, todHour)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)



allReports :: (Monad m, MonadReader Database m) =>
    POSIXTime
    -> POSIXTime
    -> TimeZone
    -> m Report
allReports start end tz = do
    db <- ask
    let cats = categories db
        datum = logs db
        categoryReport = runCategoryReport start end datum
        timeOfDayReport = runTimeOfDayReport start end tz datum
        categorySizeReport = runSizeReport start end datum
    pure Report {
        startTime = start,
        endTime = end,
        timeOfDayReport,
        categoryReport,
        categorySizeReport
    }

runCategoryReport ::
    POSIXTime
    -> POSIXTime
    -> Seq LogEntry
    -> M.Map Category NominalDiffTime
runCategoryReport sts end =
    foldr addRecord M.empty
    where
        addRecord (LogEntry {cat, start, durationSecs}) acc
            | (fromInteger start) > sts && (fromInteger start) <= end = M.insertWith (+) cat (fromMaybe 0 durationSecs) acc
            | otherwise = acc

runTimeOfDayReport ::
    POSIXTime
    -> POSIXTime
    -> TimeZone
    -> Seq LogEntry
    -> M.Map TimeOfDay NominalDiffTime
runTimeOfDayReport sts end tz =
    foldr addRecord M.empty
    where
        addRecord (LogEntry {start, durationSecs}) acc
            | (fromInteger start) > sts && (fromInteger start) <= end = M.insertWith (+) (computeTimeOfDay tz start) (fromMaybe 0 durationSecs) acc
            | otherwise = acc

runSizeReport ::
    POSIXTime
    -> POSIXTime
    -> Seq LogEntry
    -> SizeReport
runSizeReport sts end rawData = SizeReport 0 0.0 0.0

computeTimeOfDay ::
    TimeZone
    -> POSIXTime
    -> TimeOfDay
computeTimeOfDay tz ts
    | (hour > 0 && hour < 7) || hour >= 22 = LateNight
    | hour >= 7 && hour < 11 = Morning
    | hour >= 11 && hour < 2 = MidDay
    | hour >= 2 && hour < 6 = Afternoon
    | hour >= 6 && hour < 22 = Evening
    | otherwise = error "impossible hour"
    where
    hour = todHour $ localTimeOfDay lts
    lts = utcToLocalTime tz $ posixSecondsToUTCTime ts
