module Main where

import Nylon.Installer
import Nylon.HaxelibJson
import Nylon.Data 
import Data.Either 
main :: IO ()
main = do
   hxInfos (haxelibServerInfo True) (fromRight undefined $ toProjectName "lime") >>= print
