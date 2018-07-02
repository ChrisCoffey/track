module Main where

import Core
import Commands (defaultParser)
import Evaluator (evaluate)
import Storage (loadDb, saveDb, initializeDb)

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Writer (execWriterT, WriterT)
import Control.Monad.Except (runExceptT, ExceptT)
import Data.Either (either)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

type TrackM = ReaderT Database (WriterT [String] (ExceptT TTError IO)) ()

main :: IO ()
main = do
    opts <- defaultParser
    let dbf = dbFile opts
    _ <- initializeIfNecessary dbf
    rawDb <- runExceptT $ loadDb dbf
    db <- either (\e -> (print e) *> exitFailure) pure rawDb
    let run = saveDb dbf =<< evaluate opts :: TrackM
    partial <- runExceptT . execWriterT $ runReaderT run db
    case partial of
        Left  e -> print e *> exitFailure
        Right logs ->
            print logs

initializeIfNecessary ::
    FilePath
    -> IO ()
initializeIfNecessary dbPath = do
    yes <- doesFileExist dbPath
    if yes
    then pure ()
    else initializeDb dbPath
