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
import Data.Sequence ((|>))

-- Tracking requires the app to be running. There may only be a single active tracking
-- event at a time

class Monad m => MonadTracking m where
    -- | Begin tracking a category
    startTracking :: Category -> Maybe BS.ByteString -> m Database
    changeDetails :: BS.ByteString -> m Database
    stopTracking :: m Database

class MonadTime m where
    nowSeconds :: m Integer

instance (Monad m, MonadIO m) => MonadTime m where
    nowSeconds =(fromIntegral . floor) <$> liftIO getPOSIXTime

instance (Monad m, MonadReader Database m, MonadIO m, MonadError TTError m, MonadTime m) =>
    MonadTracking m where
    startTracking cat perhapsDescription = do
        db <- ask
        when (isJust $ currentActivity db) . throwError $
            UserError "There is an activity in progress already. You can't do two things at once."
        when (cat `notElem` categories db) . throwError $
            UserError "Unknown category. Add to your list before tracking."
        now <- nowSeconds
        pure $ db {currentActivity = Just LogEntry
                {
                    cat,
                    start = now,
                    details = fromMaybe "" perhapsDescription,
                    durationSecs = Nothing
                }
            }

    changeDetails newDetails = do
        db <- ask
        ca <- maybe (throwError $ UserError "No active activity to update. Start tracking something first.") pure $ currentActivity db
        pure $ db {currentActivity = Just $ ca {details = newDetails}}

    stopTracking = do
        db <- ask
        now <- nowSeconds
        ca <- maybe (throwError $ UserError "No active activity to terminate. Start tracking something first.") pure $ currentActivity db
        let lgs = logs db
            startTime = start ca
            duration = fromIntegral $ now - startTime
            ca' = ca {durationSecs = Just duration}
            db' = db {
                logs = lgs |> ca',
                currentActivity = Nothing
                }
        pure db'
