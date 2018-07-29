module Commands (
    defaultParser
) where

import Core

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Time.LocalTime ( localTimeToUTC)
import Data.Time.Format (defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Text.Parsec (parse, sepBy, many1)
import Text.Parsec.Char (digit, char, letter, space)
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
    logsCmd = analyzeLogsCommand <|> deleteLogsCommand <|> editLogCommand <|> previewLogsCommand


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

editLogCommand :: Parser InputCommand
editLogCommand =
    subparser $ command "edit" (info (editDetails <|> editSplit) $ progDesc "Edit the most recent log entry in case you made a mistake.")
    where
        editSplit =
            EditSplit
            <$> option readSplitPair (
                long "splits" <>
                short 's' <>
                help "A list of Category,Duration pairs to split the log entry into. Once the total duration has been consumed any following pairs will be dropped"
                )

        readSplitPair :: ReadM [(Category,Int)]
        readSplitPair = eitherReader $ \str ->
            case parse splitPairP "" str of
               Left err -> Left $ show err
               Right res -> Right res

        splitPairP = flip sepBy space $ do
            lbl <- many1 letter
            _   <- char ','
            dur <- read <$> many1 digit
            let cat = BSC.pack lbl
            pure (Category cat, dur)

        editDetails =
            EditDetails
            <$> option auto (
                long "duration" <>
                short 'd' <>
                help "The new duration for the log entry." <>
                value (Nothing :: Maybe Int)
                )
            <*>
                fmap (fmap Category) (
                    option auto (
                        long "category" <>
                        short 'c' <>
                        help "The new category for the log entry" <>
                        value (Nothing :: Maybe BS.ByteString)
                ))


previewLogsCommand :: Parser InputCommand
previewLogsCommand =
    subparser $ command "preview" (info parsePreview $
        progDesc "Shows all of the past logs. In the future this will paginate, filter by category, and filter by time.")
    where
        parsePreview = pure $ PreviewLogs Nothing

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
