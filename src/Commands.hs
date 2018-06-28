module Commands where

import Core

import GHC.Generics (Generic)

data InputCommands
    = StartTracking Category
    | StopTracking
    | SwitchTask {oldCat :: Category, newCat :: Category}
    | DefineCategory Category
    | ListCategories
    | ChangeCategoryName Category Category
    deriving (Eq, Show, Generic)

