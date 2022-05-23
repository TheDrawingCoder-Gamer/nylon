{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Nylon.ArgParser where 

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit qualified as CAE
import Data.Text qualified as T
import Nylon.SemVer
import Text.Megaparsec (parseMaybe)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
data Options = 
    OptInstall {
        optLib :: String
        ,optVersion :: Maybe String
    }
    | OptInfo { 
        optLib :: String
    } deriving Data
data SaneOptions = 
    SOptInstall 
        { soptLib :: T.Text
        , soptVersion :: Maybe SemVer
        }
    | SOptInfo { 
        soptLib :: T.Text
    }

piportil :: Options -> SaneOptions
piportil OptInstall{..} = SOptInstall (T.pack optLib) (optVersion <&> fromJust . parseMaybe parseSemVer)
piportil OptInfo{..} = SOptInfo (T.pack optLib)
goodArgs = fmap piportil $ cmdArgs $ modes [OptInstall {
                                optLib = def &= argPos 0 &= typ "LIBNAME", 
                                optVersion = def &= explicit &= name "libversion" &= name "lv" &= typ "VERSION"} &= name "install"
                            ,OptInfo{} &= name "info"] &= verbosity 
                            
