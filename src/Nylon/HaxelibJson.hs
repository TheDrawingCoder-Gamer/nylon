{-# LANGUAGE DeriveGeneric, DerivingVia, GeneralizedNewtypeDeriving #-}
module Nylon.HaxelibJson 
    (ProjectName,
    unProjectName,
    toProjectName,
    License(..),
    HaxelibJson(..)) where 

import Data.Text qualified as T
import Data.Char (isDigit, isAscii, isLetter)
import Nylon.SemVer
import Data.Map.Strict qualified as M
import Data.List.NonEmpty
import GHC.Generics (Generic)
import Generic.Data 
import Data.Aeson (FromJSON, ToJSON)
newtype ProjectName = ProjectName T.Text
    deriving (FromJSON, ToJSON, Eq, Ord)
unProjectName (ProjectName x) = x 

toProjectName :: T.Text -> Either T.Text ProjectName 
toProjectName name 
    | T.length name <= 3 = Left $ name <> " is too short"
    | not (T.all isAsciiLetter name) = Left $ name <> " has invalid characters"
    | any (`T.isInfixOf` name) ["haxe", "all"] = Left $ name <> " contains a reserved word"
    | any (`T.isSuffixOf` name) [".zip", ".hxml"] = Left $ name <> " ends with a reserved suffix"
    | otherwise = Right $ ProjectName name 
isAsciiLetter x = isDigit x || (isLetter x && isAscii x) || x == '.' || x == '_' || x == '-'
data License
    = GPL 
    | LGPL
    | MIT
    | BSD
    | Public 
    | Apache
    deriving (Generic)
instance FromJSON License
data HaxelibJson = HaxelibJson
    { name         :: ProjectName 
    , url          :: Maybe T.Text 
    , description  :: Maybe T.Text
    , license      :: License 
    , version      :: SemVer
    , classPath    :: Maybe T.Text 
    , releasenote  :: T.Text
    , contributors :: NonEmpty T.Text
    , tags         :: Maybe [T.Text]
    , dependencies :: Maybe (M.Map T.Text SemVer)
    , main         :: Maybe T.Text }  
    deriving Generic
instance FromJSON HaxelibJson
