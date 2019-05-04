module Management (
    Page(..),
    MonadManagement(..),
) where

import Core

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Foldable (toList)
import Data.Time.Clock.POSIX (POSIXTime)
import Numeric.Natural
import qualified Data.Sequence as S

-- | Represents a single page of data
newtype Page a = Page {vals :: [a]}

-- | A class for handling the meta operations on the database.
class MonadManagement m where
    -- | Remove all logs messages matching the provided filter. Filters are specified on the
    -- command line.
    deleteLogs :: (LogEntry -> Bool) -> m Database
    -- | Return a page of activity logs
    previewLogs :: POSIXTime -> m (Page LogEntry)

instance (Monad m, MonadReader Database m) =>
    MonadManagement m where
    deleteLogs predicate = do
        db <- ask
        let initialLogs = logs db
            afterFilter = S.filter (not . predicate) initialLogs
            db' = db {logs = afterFilter}
        pure db'

    -- Initially, just put the entire page in here. Lets see how it looks
    previewLogs since = do
        db <- ask
        let baseLogs = logs db
            sinceTime = toList $ S.filter ((> since) . fromIntegral . start) baseLogs
        pure $ Page sinceTime
