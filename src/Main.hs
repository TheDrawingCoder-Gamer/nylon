module Main where

import Nylon.Installer
import Nylon.HaxelibJson
import Nylon.Data 
import Data.Either 
import Data.Text.IO qualified as TIO
main :: IO ()
main = do
   infos <- hxInfos (haxelibServerInfo True) (fromRight undefined $ toProjectName "lime")
   case infos of 
      Left e -> 
         TIO.putStrLn e
      Right h -> 
         TIO.putStrLn $ prettyHaxeInfo h
