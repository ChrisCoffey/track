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

-- | Create the database if it doesn't already exist
initializeDb ::
    FilePath
    -> IO ()
initializeDb dbPath = do
    let db = Database {categories = [], logs = empty, currentActivity = Nothing }
    print (encode db)
    liftIO . BS.writeFile dbPath $ encode db

-- | Read the database into memory
peekDb ::
    FilePath
    -> IO (Either String Database)
peekDb dbPath =
    decode <$> BS.readFile dbPath

-- | Load the database into memory and throw an error if for some reason we couldn't actually find it/ decode it
loadDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> m Database
loadDb dbPath = do
    mDatabase <- liftIO $ peekDb dbPath
    either (const . throwError $ SystemError "Could not decode database file") pure mDatabase

-- | Write an in-memory copy of the database back to disk
saveDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> Database
    -> m ()
saveDb dbPath =
    liftIO . BS.writeFile dbPath . encode
