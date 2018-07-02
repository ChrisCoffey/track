module Commands where

import Core

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Options.Applicative

defaultMain :: IO ()
defaultMain = do
    execParser $ info (commandParser <**> helper) fullDesc
    print "Not Implemented"

data Options =
    Opts {
        cmd :: InputCommand,
        silent :: Bool,
        dbFile :: FilePath
    } deriving (Show)

data InputCommand
    = StartTracking Category (Maybe BS.ByteString)
    | StopTracking
    | SwitchTask Category
    | DefineCategory Category
    | ListCategories
    | ChangeCategoryName Category Category
    deriving (Eq, Show, Generic)


commandParser :: Parser InputCommand
commandParser = hsubparser $
    command "start" (info startCommand $ progDesc "Start tracking time against a particular category of work")
    <>
    command "stop" (info stopCommand $ progDesc "Stop tracking time. Generally useful at the end of the day")
    <>
    command "switch" (info stopCommand $ progDesc "Switch between two tasks. This stops the current task and starts a new task")
    <>
    command "category" (info categoryCmd $ progDesc "Manage the configured categories")
    where
    categoryCmd = newCategoryCommand <|> listCatsCommand <|> modifyCategoryCmd


startCommand :: Parser InputCommand
startCommand =
    StartTracking <$> (
    Category
    <$> strOption
        (long "category" <>
        short 'c' <>
        metavar "CATEGORY" <>
        help "The Category to track against"
        )
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

emptyStringToNothing :: (Eq s, IsString s) =>
    s
    -> Maybe s
emptyStringToNothing "" = Nothing
emptyStringToNothing s = Just s

stopCommand :: Parser InputCommand
stopCommand = undefined

switchCommand :: Parser InputCommand
switchCommand = undefined

newCategoryCommand :: Parser InputCommand
newCategoryCommand = undefined

listCatsCommand :: Parser InputCommand
listCatsCommand = undefined

modifyCategoryCmd :: Parser InputCommand
modifyCategoryCmd = undefined
