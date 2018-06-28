module Core (
    Category(..),
    LogEntry(..),
    Database(..),
    Context(..),
    initializeContext
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
        logs :: Seq LogEntry
        }
    deriving (Eq, Ord, Show, Serialize, Generic)

data TTError
    = SystemError T.ByteString
    | UserError T.ByteString
    deriving (Show)

data Context =
    Ctx {
       currentLogEntry :: MVar LogEntry,
       state :: Database
    }

initializeContext :: MonadIO m =>
    Database
    -> m Context
initializeContext db = do
    cell <- liftIO newEmptyMVar
    pure Ctx {
        state = db,
        currentLogEntry = cell
        }

type TTM = ReaderT Context (ExceptT TTError IO)
