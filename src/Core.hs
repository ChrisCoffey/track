module Core (
    Category(..),
    LogEntry(..),
    Database(..),
    TTError(..),
    Options(..),
    InputCommand(..),

    defaultDbFile
)
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Sequence (Seq)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.ByteString as T

defaultDbFile :: FilePath
defaultDbFile = "/usr/local/bin/.timeDb"

newtype Category = Category {catName :: T.ByteString}
    deriving (Eq, Ord, Show, Serialize, Generic)

data LogEntry =
    LogEntry {cat:: Category,
              details:: T.ByteString,
              start :: Integer,
              durationSecs :: Maybe Int }
    deriving (Eq, Ord, Show, Serialize, Generic)

data Database =
    Database {
        categories :: [Category],
        logs :: Seq LogEntry,
        currentActivity :: Maybe LogEntry
        }
    deriving (Eq, Ord, Show, Serialize, Generic)

data TTError
    = SystemError T.ByteString
    | UserError T.ByteString
    deriving (Show)


data Options =
    Opts {
        cmd :: InputCommand,
        silent :: Bool,
        dbFile :: FilePath
    } deriving (Show)

data InputCommand
    = StartTracking Category (Maybe T.ByteString)
    | StopTracking
    | SwitchTask Category (Maybe T.ByteString)
    | DefineCategory Category
    | ListCategories
    | ChangeCategoryName Category Category
    deriving (Eq, Show, Generic)
