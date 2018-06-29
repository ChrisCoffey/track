module Tracking where

import Core
import Storage (defaultDbFile, saveDb)

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
    startTracking :: Category -> Maybe BS.ByteString -> m ()
    changeDetails :: BS.ByteString -> m ()
    stopTracking :: m LogEntry


instance (Monad m, MonadReader Database m, MonadIO m, MonadError TTError m) =>
    MonadTracking m where
    startTracking cat perhapsDescription = do
        db <- ask
        when (isJust $ currentActivity db) . throwError $ UserError "There is an activity in progress already. You can't do two things at once."
        now <- nowSeconds
        let db' = db {currentActivity = Just LogEntry
                {
                    cat,
                    start = now,
                    details = fromMaybe "" perhapsDescription,
                    durationSecs = Nothing
                }
            }
        saveDb defaultDbFile db'

    changeDetails newDetails = do
        db <- ask
        ca <- maybe (throwError $ UserError "No active activity to update. Start tracking something first.") pure $ currentActivity db
        let db' = db {currentActivity = Just $ ca {details = newDetails}}
        saveDb defaultDbFile db'

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
        saveDb defaultDbFile db'
        pure ca'

nowSeconds :: MonadIO m => m Integer
nowSeconds = (fromIntegral . floor) <$> liftIO getPOSIXTime
