module Commands (
    defaultParser
) where

import Core

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import qualified Data.ByteString as BS
import Data.Time.LocalTime ( localTimeToUTC)
import Data.Time.Format (defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Options.Applicative

defaultParser :: IO Options
defaultParser =
    execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser =
    Opts
    <$> commandParser
    <*> flag False False (long "quiet" <> short 'q' <> help "Silence program output")
    <*> strOption (long "dbPath" <> short 'd' <> metavar "DATABASE FILE" <> value defaultDbFile)

commandParser :: Parser InputCommand
commandParser = hsubparser $
    command "start" (info startCommand $ progDesc "Start tracking time against a particular category of work")
    <>
    command "stop" (info stopCommand $ progDesc "Stop tracking time. Generally useful at the end of the day")
    <>
    command "switch" (info switchCommand $ progDesc "Switch between two tasks. This stops the current task and starts a new task")
    <>
    command "category" (info categoryCmd $ progDesc "Manage the configured categories")
    <>
    command "logs" (info logsCmd $ progDesc "Manage your time logs")
    where
    categoryCmd = newCategoryCommand <|> listCatsCommand <|> renameCategoryCmd
    logsCmd = analyzeLogsCommand <|> deleteLogsCommand


startCommand :: Parser InputCommand
startCommand =
    StartTracking <$>
    (
    Category <$> argument str (metavar "Category" <>
                               help "The Category to track against" <>
                               value "")
    )
    <*>
    ( emptyStringToNothing
    <$> strOption
        (long "desc" <>
        short 'd' <>
        metavar "DESCIRPTION" <>
        help "An optional description for the task" <>
        value ""
    )
    )


stopCommand :: Parser InputCommand
stopCommand = pure StopTracking

switchCommand :: Parser InputCommand
switchCommand =
    SwitchTask <$>
    (
    Category <$> argument str (metavar "Category" <>
                               help "The Category to track against" <>
                               value "")
    )
    <*>
    ( emptyStringToNothing
    <$> strOption
        (long "desc" <>
        short 'd' <>
        metavar "DESCIRPTION" <>
        help "An optional description for the task" <>
        value ""
        )
    )

--
-- Category commands
--

newCategoryCommand :: Parser InputCommand
newCategoryCommand =
    subparser $ command "new" (info parseDefCat $ progDesc "Define a new category")
    where
    parseDefCat =
        DefineCategory
        <$> (Category <$> argument str (metavar "Category" <>
                                        help "The name of the new category you'd like to create")
            )

listCatsCommand :: Parser InputCommand
listCatsCommand =
    subparser $ command "list" (info (pure ListCategories) $ progDesc "List the currently configured categories")

renameCategoryCmd :: Parser InputCommand
renameCategoryCmd =
    subparser $ command "rename" (info parseDefCat $ progDesc "Rename a new category")
    where
    parseDefCat =
        ChangeCategoryName
        <$> (Category <$> argument str (metavar "Category" <>
                                        help "The name of the new category you'd like to rename")
            )
        <*>
            (Category <$> argument str (metavar "Category" <>
                                        help "The new category name")
            )

--
-- Logs
--

analyzeLogsCommand ::
    Parser InputCommand
analyzeLogsCommand =
    subparser $ command "analyze" (info parseAnalyze $ progDesc "Analyze your log activity. Prints the results to a file.")
    where
    parseAnalyze =
        Analyze
        <$> option (Just <$> maybeReader parseTS)( long "start" <>
                                          short 's' <>
                                          metavar "START TIME" <>
                                          help "The time to begin analyzing logs from" <>
                                          value (Nothing :: Maybe POSIXTime)
                                        )
        <*> option (Just <$> maybeReader parseTS)( long "end" <>
                                          short 'e' <>
                                          metavar "END TIME" <>
                                          help "Ignore logs after this time" <>
                                          value (Nothing :: Maybe POSIXTime)
                                        )
        <*> strOption (long "filePath" <>
                       short 'f' <>
                       metavar "OUTPUT FILE" <>
                       help "Where to write the results of your time log analysis"
                       )


deleteLogsCommand :: Parser InputCommand
deleteLogsCommand =
    subparser . command "delete" $ info ((DeleteByCategory <$> byCategory) <|> byTime) $
        progDesc "Delete log files based on time or Category"
    where
        byTime =
            DeleteByTime
            <$> option (Just <$> maybeReader parseTS)( long "start" <>
                                            short 's' <>
                                            metavar "START TIME" <>
                                            help "Delete logs that start after this time" <>
                                            value (Nothing :: Maybe POSIXTime)
                                            )
            <*> option (Just <$> maybeReader parseTS)( long "end" <>
                                            short 'e' <>
                                            metavar "END TIME" <>
                                            help "Delete logs starting beore this time" <>
                                            value (Nothing :: Maybe POSIXTime)
                                            )

        byCategory = Category <$> strOption (long "cat" <>
                                short 'c' <>
                                metavar "CATEGORY" <>
                                help "The category of data to delete")

previewLogsCommand :: Parser InputCommand
previewLogsCommand = undefined

--
-- Utility functions
--

emptyStringToNothing :: (Eq s, IsString s) =>
    s
    -> Maybe s
emptyStringToNothing "" = Nothing
emptyStringToNothing s = Just s

parseTS :: String -> Maybe POSIXTime
parseTS s = let
    withTime = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") s
    noTime = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) s
    in utcTimeToPOSIXSeconds <$> (withTime <|> noTime)
