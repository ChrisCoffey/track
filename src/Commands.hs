module Commands (
    defaultParser
) where

import Core

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import qualified Data.ByteString as BS
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
    where
    categoryCmd = newCategoryCommand <|> listCatsCommand <|> renameCategoryCmd


startCommand :: Parser InputCommand
startCommand =
    StartTracking <$>
    (
    Category <$> argument str (metavar "Category" <>
                               help "The Category to track against")
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
                               help "The Category to track against")
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

emptyStringToNothing :: (Eq s, IsString s) =>
    s
    -> Maybe s
emptyStringToNothing "" = Nothing
emptyStringToNothing s = Just s
