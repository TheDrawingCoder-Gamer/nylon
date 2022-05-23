module Main where

import Nylon.Installer
import Nylon.HaxelibJson
import Nylon.Data
import Nylon.MetaData
import Data.Either 
import Data.Text.IO qualified as TIO
import Nylon.SemVer
import Nylon.ArgParser
import Data.Text qualified as T
import Data.Maybe (fromJust)
import System.Directory (createDirectoryIfMissing)
main :: IO ()
main = do
   args <- goodArgs
   case args of 
      SOptInstall lib ver -> do
         createDirectoryIfMissing True "./haxe_libraries"
         TIO.putStrLn $ "Installing " <> lib
         goodVer <- case ver of 
                        Just v ->
                           pure v 
                        Nothing -> do
                           fromJust . getLatest . fromRight undefined <$> hxInfos (haxelibServerInfo True) (fromRight undefined $ toProjectName lib)
         hxDownloadHaxelib (haxelibServerInfo True) (fromRight undefined $ toProjectName lib) goodVer (T.unpack ("./haxe_libraries/" <> lib))
      SOptInfo lib -> do 
         infos <- hxInfos (haxelibServerInfo True) (fromRight undefined $ toProjectName lib)
         case infos of 
            Left e -> 
               TIO.putStrLn e
            Right h -> 
               TIO.putStrLn $ prettyHaxeInfo h
               
