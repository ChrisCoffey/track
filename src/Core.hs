module Core where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
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

-- | Represents a category of work that can be tracked. Right now names are their own keys.
newtype Category = Category {catName :: T.ByteString}
    deriving (Eq, Ord, Show, Serialize, Generic, ToJSONKey)

instance ToJSON Category where
    toJSON = toJSON . BSC.unpack . catName

-- | An activity entry.
data LogEntry =
    LogEntry {cat:: Category,
              details:: T.ByteString,
              start :: Integer,
              durationSecs :: Maybe Int }
    deriving (Eq, Ord, Show, Serialize, Generic)

-- | The activity log, along with the set of supported 'Category' values
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

-- | Parsed command line options and flags
data Options =
    Opts {
        cmd :: InputCommand,
        silent :: Bool,
        dbFile :: FilePath
    } deriving (Show)

-- | The commands track supports
data InputCommand
    = StartTracking Category (Maybe T.ByteString)
    | StopTracking
    | SwitchTask Category (Maybe T.ByteString)
    | DefineCategory Category
    | ListCategories
    | ChangeCategoryName Category Category
    | Analyze {
        aStart :: Maybe POSIXTime,
        aEnd :: Maybe POSIXTime,
        aPath :: FilePath
        }
    | DeleteByTime {
        dStart :: Maybe POSIXTime,
        dEnd :: Maybe POSIXTime
        }
    | DeleteByCategory Category
    | PreviewLogs (Maybe Category)
    | EditDetails (Maybe Int) (Maybe Category)
    | EditSplit [(Category, Int)]
    | Version T.ByteString
    deriving (Eq, Show, Generic)

-- | Represents the full set of reports that track can perform. This is a silly data structure right now.
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

-- | A normalized time of day. The bucket sizes are not equal, but instead equate to the times
-- that I perceive the term to mean.
data TimeOfDay
    = LateNight
    | Morning
    | MidDay
    | Afternoon
    | Evening
    deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey)

-- | A helper that prints the pretty version of a log entry out, given the user's current timezone
prettyPrintLogEntry ::
    TimeZone
    -> LogEntry
    -> T.ByteString
prettyPrintLogEntry tz le =
    ts <> " : " <> catName (cat le) <> " for " <> toFriendlyTime <> ". Details "<> details le
    where
        ts = BSC.pack . show . utcToLocalTime tz . posixSecondsToUTCTime . fromIntegral $ start le
        d = fromMaybe 0 $ durationSecs le
        toFriendlyTime = let
            hrs = d `div` 3600
            mins = (d `mod` 3600) `div` 60
            in BSC.pack (show hrs <>":"<>show mins)
