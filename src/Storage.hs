module Storage (
    defaultDbFile,
    loadDb,
    saveDb,
    initializeDb
) where

import Core

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Data.Serialize (encode,decode)
import qualified Data.ByteString as BS
import Data.Either (either)
import Data.Sequence (empty)

defaultDbFile :: FilePath
defaultDbFile = "/Users/ccoffey/.timeDb"

initializeDb ::
    FilePath
    -> IO ()
initializeDb dbPath = do
    let db = Database {categories = [], logs = empty, currentActivity = Nothing }
    print (encode db)
    liftIO . BS.writeFile dbPath $ encode db

loadDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> m Database
loadDb dbPath = do
    rawBytes <- liftIO $ BS.readFile dbPath
    let mDatabase = decode rawBytes
    either (const . throwError $ SystemError "Could not decode database file") pure mDatabase

saveDb :: (MonadIO m, MonadError TTError m) =>
    FilePath
    -> Database
    -> m ()
saveDb dbPath =
    liftIO . BS.writeFile dbPath . encode
