{-# LANGUAGE RecordWildCards #-}
module Nylon.Data where 

import Network.HTTP.Req
import Data.Text (Text)
import Data.Text qualified as T
import Data.Bifunctor qualified as BF
import Control.Category ((>>>))
data CheckLevel 
    = NoCheck
    | CheckSyntax
    | CheckData
    deriving (Eq, Ord, Enum, Bounded, Show)

data ServerInfo = ServerInfo 
    { siHost       :: Text
    , siDir        :: Text
    , siUrl        :: Text
    , siApiVersion :: Text
    , siUseSsl     :: Bool
    }
    deriving (Eq, Show)

haxelibServerInfo = ServerInfo 
    "lib.haxe.org"
    ""
    "index.n"
    "3.0"

siteUrl :: ServerInfo -> Either (Url Http) (Url Https) 
siteUrl ServerInfo{..} =
    BF.bimap addDirIfNeeded addDirIfNeeded $ (if siUseSsl then Right . https else Left . http) siHost
    where 
        addDirIfNeeded :: Url a -> Url a 
        addDirIfNeeded = 
            if T.null siDir then 
                id 
            else 
                (/: siDir)

remotingUrl :: ServerInfo -> Either (Url Http) (Url Https)
remotingUrl si@ServerInfo{..} = 
    addJunk <$> siteUrl si
    where
    addJunk :: Url a -> Url a
    addJunk = (/: "api") >>> (/: siApiVersion) >>> (/: siUrl)
repository :: Text
repository = "files/3.0"
