module Management (
    MonadManagement(..),
) where

import Core

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Time.Clock.POSIX (POSIXTime)
import Numeric.Natural
import qualified Data.Sequence as S

newtype Page a = Page {vals :: [a]}

class MonadManagement m where
    deleteLogs :: (LogEntry -> Bool) -> m Database
    previewLogs :: POSIXTime -> m (Page LogEntry)

instance (Monad m, MonadReader Database m) =>
    MonadManagement m where
    deleteLogs predicate = do
        db <- ask
        let initialLogs = logs db
            afterFilter = S.filter (not . predicate) initialLogs
            db' = db {logs = afterFilter}
        pure db'

    previewLogs = undefined
