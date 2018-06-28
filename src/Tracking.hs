module Tracking where

import Core
import Control.Monad.Trans (MonadIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as BS

-- Tracking requires the app to be running. There may only be a single active tracking
-- event at a time

class Monad m => MonadTracking m where
    -- | Begin tracking a category
    startTracking :: Category -> Maybe BS.ByteString -> m ()
    changeDetails :: BS.ByteString -> m ()
    stopTracking :: m LogEntry


instance (MonadReader Context m, MonadIO m, MonadError TTError m) =>
    MonadTracking m where
    startTracking = do
        -- validate that the mvar is empty
        -- if its empty, create a new log entry, with or without the description as provided
    changeDetails :: BS.ByteString -> m ()
        -- if the MVar is full, pull it out, update the descriptoin, then put it back
    stopTracking :: m LogEntry
        -- Set the duration on the current log entry & clear the mvar
        -- return the log entry
