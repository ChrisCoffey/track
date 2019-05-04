module Reports (
    allReports,
    runTimeOfDayReport,
    runCategoryReport,
    runContextSwitchReport
) where

import Core
import Control.Monad.Reader (MonadReader, ask)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime, localTimeOfDay, todHour)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq

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
        window = ReportWindow {startTime = start, endTime = end}
        categoryReport = runCategoryReport start end datum
        timeOfDayReport = runTimeOfDayReport start end tz datum
        contextSwitchReport = runContextSwitchReport window tz datum
    pure Report {
        window,
        timeOfDayReport,
        categoryReport,
        contextSwitchReport
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
        addRecord LogEntry {cat, start, durationSecs} acc
            | fromInteger start > sts && fromInteger start <= end =
                M.insertWith (+)
                             cat
                             (fromIntegral durationSecs)
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
        addRecord LogEntry {start, durationSecs} acc
            | fromInteger start > sts && fromInteger start <= end =
                M.insertWith (+)
                             (computeTimeOfDay tz (fromInteger start))
                             (fromIntegral durationSecs)
                             acc
            | otherwise = acc

-- | I've waffled between 30 & 45 minutes, but settled on 30 minute for now.
shortDuration :: NominalDiffTime
shortDuration = 1800

-- | Calculate the details around context switching. For my own purposes, a context switch is anytime that you spend
-- less than 45 minutes on a task. Therefore, if you're jumping between many different categories of work, consider
-- creating a "misc" or "filler" category to track this as a single block of time.
--
-- The "filler" approach is particularly useful when there are only 20 minutes between meetings.
runContextSwitchReport ::
    ReportWindow
    -> TimeZone
    -> Seq LogEntry
    -> ContextSwitchReport
runContextSwitchReport window tz logs = let
    (_ :< rest) = Seq.viewl logs
    pairs = logs `Seq.zip` rest
    switches = Seq.filter isShort pairs
    in foldr accumulate
             (ContextSwitchReport {
                total = 0,
                byTimeOfDay = M.empty,
                interruptByCategory = M.empty,
                interruptedByCategory = M.empty
              })
             switches
    where
        -- Some basic helpers
        isShort (LogEntry {durationSecs}, _) = fromIntegral durationSecs <= shortDuration
        toTimeOfDay = computeTimeOfDay tz . fromInteger . start

        -- The folding function to compute the report
        accumulate (interrupted, interrupter) report = report {
            total = total report + 1,
            byTimeOfDay = M.insertWith (+) (toTimeOfDay interrupted) 1 (byTimeOfDay report),
            interruptByCategory = M.insertWith (+) (cat interrupted) 1 (interruptByCategory report),
            interruptedByCategory = M.insertWith (+) (cat interrupter) 1 (interruptedByCategory report)
            }


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
