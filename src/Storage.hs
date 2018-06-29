module Storage (
    defaultDbFile,
    loadDb,
    saveDb
) where

import Core

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Data.Serialize (encode,decode)
import Data.ByteString as BS
import Data.Either (either)

defaultDbFile :: FilePath
defaultDbFile = "~/.timeDb"

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
