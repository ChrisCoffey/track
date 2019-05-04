module Evaluator (
    MonadInput(..),
    evaluate
) where

import Core
import qualified Tracking as TR
import qualified Reports as R
import qualified Management as Mgmt

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ask, local, asks)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Data.Semigroup ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Char8 as BSSC
import Data.Foldable (traverse_)
import System.IO (putStrLn, getLine)

-- | This class represents the interactive elements of track's operation
class Monad m => MonadInput m where
    chooseOne :: Show a => [a] -> m a

instance (Monad m, MonadIO m ) => MonadInput m where
    chooseOne choices = do
        let choiceMessages = asDisplayChoice <$> zipwithIndex choices
        traverse_ (liftIO . putStrLn) choiceMessages
        n <- read <$> liftIO getLine
        pure ( choices !! n )
        where
        zipwithIndex = zip [0..]

        asDisplayChoice (i,v) =
            show i <> " ) "<> show v

-- | For handling output from track. Used for dumping reports to json currently
class Monad m => MonadOutput m where
    writeBSCToFile :: FilePath -> BSC.ByteString -> m ()

instance (Monad m, MonadIO m) => MonadOutput m where
    writeBSCToFile fp bytes = liftIO $ BSC.writeFile fp bytes

type EvalM m = (Monad m, MonadReader Database m, MonadError TTError m)

-- | The main function. This applies the command line arguments to the current database state
-- and returns any relevant results.
evaluate :: (EvalM m, MonadWriter [String] m, MonadOutput m,
            TR.MonadTracking m, MonadInput m, TR.MonadTime m)=>
    Options
    -> m Database
evaluate o@(Opts {cmd, silent}) =
    case cmd of
        StartTracking (Category "") -> do
            cats <- asks categories
            cat <- chooseOne cats
            evaluate (o {cmd= StartTracking cat} )
        StartTracking cat -> TR.startTracking cat
        StopTracking -> TR.stopTracking
        SwitchTask cat -> do
            db' <- evaluate (o {cmd=StopTracking})
            local (const db') $ evaluate (o {cmd=StartTracking cat})

        DefineCategory cat -> defineCategory cat
        ListCategories -> listCategories
        ChangeCategoryName oldCat newCat -> changeCategoryName oldCat newCat

        Analyze {aStart, aEnd, aPath} -> do
            now <- fromInteger <$> TR.nowSeconds
            tz <- TR.getTimeZone
            reps <- R.allReports (fromMaybe 0 aStart) (fromMaybe now aEnd) tz
            writeBSCToFile aPath $ encodePretty reps
            ask
        PreviewLogs _ -> do
            Mgmt.Page preview <- Mgmt.previewLogs 0
            tz <- TR.getTimeZone
            let report = BSSC.unpack . prettyPrintLogEntry tz <$> preview
                prettyReport = intersperse "\n" report
            tell prettyReport
            ask

        DeleteByTime mStartTs mEndTs -> do
            now <- fromInteger <$> TR.nowSeconds
            let st = fromMaybe 0 mStartTs
                end = fromMaybe now mEndTs
                pred le = fromInteger (start le) >= st && fromInteger (start le) <= end
            Mgmt.deleteLogs pred
        DeleteByCategory dCat ->
            Mgmt.deleteLogs ((==) dCat . cat)
        EditDetails mDur mCat ->
            TR.modifyLogHead mDur mCat

        Version version -> do
            tell ["version: " <> BSSC.unpack version]
            ask

defineCategory :: (EvalM m) =>
    Category
    -> m Database
defineCategory c = do
    db <- ask
    let cats = categories db
    when (c `elem` cats) $
        throwError $ UserError "This category is already in the database. No need to add it."
    pure $ db {categories=c:cats}

listCategories :: (EvalM m, MonadWriter [String] m) =>
    m Database
listCategories = do
    db <- ask
    traverse_ tell $ (singleton . BSSC.unpack . catName) <$> categories db
    pure db
    where
    singleton x = [x]

changeCategoryName :: (EvalM m) =>
    Category
    -> Category
    -> m Database
changeCategoryName oldCat newCat = do
    db <- ask
    let cats = categories db
    when (oldCat `notElem` cats) .
        throwError $ UserError "Not a category in the system. Cannot rename it."
    when (newCat `elem` cats) .
        throwError $ UserError "This is already a category in the system. Cannot rename to it."

    let cats' = (\c -> if c == oldCat then newCat else c) <$> cats
    pure $ db {categories = cats'}
