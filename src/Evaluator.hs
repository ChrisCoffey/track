module Evaluator (
    MonadInput(..),
    evaluate
) where

import Core
import qualified Tracking as TR

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Semigroup ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (traverse_)
import System.IO (putStrLn, getLine)

class Monad m => MonadInput m where
    chooseOne :: Show a => [a] -> m a

instance (Monad m, MonadIO m ) => MonadInput m where
    chooseOne choices = do
        let choiceMessages = asDisplayChoice <$> zipwithIndex choices
        traverse (liftIO . putStrLn) choiceMessages
        n <- read <$> liftIO getLine
        pure . head $ drop n choices
        where
        zipwithIndex = zip [0..]

        asDisplayChoice (i,v) =
            show i <> " ) "<> show v

type EvalM m = (Monad m, MonadReader Database m, MonadError TTError m)

evaluate :: (EvalM m, MonadWriter [String] m, TR.MonadTracking m, MonadInput m )=>
    Options
    -> m Database
evaluate o@(Opts {cmd, silent}) =
    case cmd of
        StartTracking (Category "") mDesc -> do
            cats <- categories <$> ask
            cat <- chooseOne cats
            evaluate (o {cmd=StartTracking cat mDesc} )
        StartTracking cat mDesc -> TR.startTracking cat mDesc
        StopTracking -> TR.stopTracking
        SwitchTask cat mDesc -> do
            db' <- evaluate (o {cmd=StopTracking})
            local (const db') $ evaluate (o {cmd=StartTracking cat mDesc})
        DefineCategory cat -> defineCategory cat
        ListCategories -> listCategories
        ChangeCategoryName oldCat newCat -> changeCategoryName oldCat newCat

defineCategory :: (EvalM m) =>
    Category
    -> m Database
defineCategory c = do
    db <- ask
    let cats = categories db
    when (c `elem` cats) $
        throwError $ UserError "This category is already in the databse. No need to add it."
    pure $ db {categories=c:cats}

listCategories :: (EvalM m, MonadWriter [String] m) =>
    m Database
listCategories = do
    db <- ask
    traverse_ tell $ (singleton . BSC.unpack . catName) <$> categories db
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
