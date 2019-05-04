module Reports (
    allReports,
    runTimeOfDayReport,
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

-- | Evaluate all of the reports against the current activity database
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
    pure Report {
        startTime = start,
        endTime = end,
        timeOfDayReport,
        categoryReport
    }

-- | Calculates how much time has been spent in each category over the provided time window
runCategoryReport ::
    POSIXTime
    -> POSIXTime
    -> Seq LogEntry
    -> M.Map Category NominalDiffTime
runCategoryReport sts end =
    foldr addRecord M.empty
    where
        addRecord (LogEntry {cat, start, durationSecs}) acc
            | (fromInteger start) > sts && (fromInteger start) <= end =
                M.insertWith (+)
                             cat
                             (fromIntegral $ fromMaybe 0 durationSecs)
                             acc
            | otherwise = acc

-- | Calculates how much time has been spent in each part of the day, regardless of category. I.e. when are you doing
-- most of your work?
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
            | (fromInteger start) > sts && (fromInteger start) <= end =
                M.insertWith (+)
                             (computeTimeOfDay tz (fromInteger start))
                             (fromIntegral $ fromMaybe 0 durationSecs)
                             acc
            | otherwise = acc


-- | Translates a posix timestamp into the 'TimeOfDay' given a particular timezone.
computeTimeOfDay ::
    TimeZone
    -> POSIXTime
    -> TimeOfDay
computeTimeOfDay tz ts
    | (hour > 0 && hour < 7) || hour >= 22 = LateNight
    | hour >= 7 && hour < 11 = Morning
    | hour >= 11 && hour < 14 = MidDay
    | hour >= 14 && hour < 18 = Afternoon
    | hour >= 18 && hour < 22 = Evening
    | otherwise = error "impossible hour"
    where
    hour = todHour $ localTimeOfDay lts
    lts = utcToLocalTime tz $ posixSecondsToUTCTime ts
