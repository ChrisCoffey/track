-- | This module provides the core interface for tracking work.
-- The details of how work is tracked are explained on the 'MonadTracking' class declaration.
module Tracking where

import Core
import Storage (saveDb)

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.ByteString as BS
import Data.Maybe (isJust, fromMaybe, maybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import Data.Sequence ((|>), ViewR(..), viewr)

-- | Tracking activities is an inherently stateful operation, and this class provides the interface
-- for those operations.
--
-- When it comes to tracking activities, there are two core operations,
-- starting and stopping tracking. Tracking begins with a call to 'startTracking', after which the
-- only allowed operation is calling 'stopTracking'. The idea is that you can only do one thing at a time,
-- so there's no sense in allowing users to start multiple work items.
class Monad m => MonadTracking m where
    -- | Begin tracking a category. This is intended to modify the database.
    startTracking :: Category -> m Database
    -- | Stop tracking the currently active task. This only makes sense to call if there
    -- is an active element in the database
    stopTracking :: m Database
    -- | Change the duration or category on the most recently stored log entry.
    modifyLogHead :: Maybe Int -> Maybe Category -> m Database

-- | An abstraction for working with the system clock & timezone data.
class MonadTime m where
    nowSeconds :: m Integer
    getTimeZone :: m TimeZone

instance (Monad m, MonadIO m) => MonadTime m where
    nowSeconds = fromIntegral . floor <$> liftIO getPOSIXTime
    getTimeZone = liftIO getCurrentTimeZone

instance (Monad m, MonadReader Database m, MonadIO m, MonadError TTError m, MonadTime m) =>
    MonadTracking m where
    startTracking cat = do
        db <- ask
        when (isJust $ currentActivity db) . throwError $
            UserError "There is an activity in progress already. You can't do two things at once."
        when (cat `notElem` categories db) . throwError $
            UserError "Unknown category. Add to your list before tracking."
        now <- nowSeconds
        pure $ db {currentActivity = Just PendingEntry
                {
                    peCat = cat,
                    peStart = now
                }
            }

    stopTracking = do
        db <- ask
        now <- nowSeconds
        ca <- maybe (throwError $ UserError "No active activity to terminate. Start tracking something first.") pure $ currentActivity db
        let lgs = logs db
            startTime = peStart ca
            duration = fromIntegral $ now - startTime
            ca' = LogEntry {
                start = startTime,
                cat = peCat ca,
                durationSecs = duration
                }
            db' = db {
                logs = lgs |> ca',
                currentActivity = Nothing
                }
        pure db'

    modifyLogHead mDur mCat = do
        db <- ask
        let changeDuration d le = le {durationSecs = fromIntegral d}
            changeCategory c le = le {cat = c}
            (rest :> logHead) = viewr $ logs db
        case (mDur, mCat) of
            (Just d, Just c) -> pure $ db {logs = rest |> changeCategory c ( changeDuration d logHead ) }
            (Just d, Nothing) -> pure $ db {logs = rest |> changeDuration d logHead }
            (Nothing, Just c) -> pure $ db {logs = rest |> changeCategory c logHead }
            (Nothing, Nothing) -> pure db


