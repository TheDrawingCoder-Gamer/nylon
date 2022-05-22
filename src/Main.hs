module Main where

import Nylon.Installer
import Nylon.HaxelibJson
import Nylon.Data 
import Data.Either 
import Data.Text.IO qualified as TIO
import Nylon.SemVer

main :: IO ()
main = do
   infos <- hxInfos (haxelibServerInfo True) (fromRight undefined $ toProjectName "lime")
   case infos of 
      Left e -> 
         TIO.putStrLn e
      Right h -> 
         TIO.putStrLn $ prettyHaxeInfo h
   hxDownloadHaxelib (haxelibServerInfo True) (fromRight undefined $ toProjectName "lime") SemVer{ semMajor = 7, semMinor = 9, semPatch = 0, semPreview = Nothing, previewNum = Nothing} "./test"
