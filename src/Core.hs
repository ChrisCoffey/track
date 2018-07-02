module Core where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Sequence (Seq)
import Data.Serialize (Serialize)
import qualified Data.Map as M
import Data.Aeson (ToJSON,toJSON, ToJSONKey)
import GHC.Generics (Generic)
import Numeric.Natural
import qualified Data.ByteString as T
import qualified Data.ByteString.Char8 as BSC

defaultDbFile :: FilePath
defaultDbFile = "/usr/local/bin/.timeDb"

newtype Category = Category {catName :: T.ByteString}
    deriving (Eq, Ord, Show, Serialize, Generic, ToJSONKey)

instance ToJSON Category where
    toJSON = toJSON . BSC.unpack . catName

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
    | Analyze FilePath
    deriving (Eq, Show, Generic)

-- Represents the
data Report
    = Report {
        startTime :: POSIXTime,
        endTime :: POSIXTime,
        categoryReport :: M.Map Category NominalDiffTime,
        timeOfDayReport :: M.Map TimeOfDay NominalDiffTime,
        categorySizeReport :: M.Map Category SizeReport
        } deriving (Show, Generic, ToJSON)

data SizeReport =
    SizeReport {
        count :: Natural,
        mean :: Double,
        stdDev :: Double
    } deriving (Show, Generic, ToJSON)

data TimeOfDay
    = LateNight
    | Morning
    | MidDay
    | Afternoon
    | Evening
    deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey)
