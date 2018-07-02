module Storage (
    loadDb,
    saveDb,
    initializeDb,

    -- | Debugging function
    peekDb
) where

import Core

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Data.Serialize (encode,decode)
import qualified Data.ByteString as BS
import Data.Either (either)
import Data.Sequence (empty)

initializeDb ::
    FilePath
    -> IO ()
initializeDb dbPath = do
    let db = Database {categories = [], logs = empty, currentActivity = Nothing }
    print (encode db)
    liftIO . BS.writeFile dbPath $ encode db

peekDb ::
    FilePath
    -> IO (Either String Database)
peekDb dbPath =
    decode <$> BS.readFile dbPath

loadDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> m Database
loadDb dbPath = do
    mDatabase <- liftIO $ peekDb dbPath
    either (const . throwError $ SystemError "Could not decode database file") pure mDatabase

saveDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> Database
    -> m ()
saveDb dbPath =
    liftIO . BS.writeFile dbPath . encode
