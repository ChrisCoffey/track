module Core (
    Category(..),
    LogEntry(..),
    Database(..),
    TTError(..)
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
        logs :: Seq LogEntry,
        currentActivity :: Maybe LogEntry
        }
    deriving (Eq, Ord, Show, Serialize, Generic)

data TTError
    = SystemError T.ByteString
    | UserError T.ByteString
    deriving (Show)


type TTM = ReaderT Database (ExceptT TTError IO)
