{-# LANGUAGE RecordWildCards #-}
module Nylon.Installer where 

import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Nylon.Data
import Nylon.HaxelibJson
import Nylon.MetaData
import Nylon.Serializer
import Nylon.SemVer
import System.IO
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Binary qualified as CB
import Data.String
import Data.Bifunctor qualified as BF
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Network.HTTP.Client qualified as HC
import Control.Category ((>>>))
import Text.Megaparsec (parse, errorBundlePretty)
import Codec.Archive.Zip qualified as Z
import Data.Char (isDigit, isLetter)
hxDownload :: ServerInfo -> T.Text -> String ->  IO ()
hxDownload remote filename outPath = do 
    let fileUrl = BF.bimap addOtherThings addOtherThings $ siteUrl remote
    
    
    case fileUrl of 
        Left a -> 
            runReq defaultHttpConfig $ request mempty a
        Right a -> 
            runReq defaultHttpConfig $ request mempty a
    
    where
        addRepo :: Url a -> Url a
        addRepo x = foldl (/:) x (T.splitOn "/" repository)
        addOtherThings :: Url a -> Url a 
        addOtherThings = addRepo >>> (/: filename)
        request :: Option a -> Url a -> Req () 
        request optionals url = 
            reqBr GET url NoReqBody optionals $ \r ->
                runConduitRes $ responseBodySource r .| do 
                    bs <- C.sinkLazy
                    let archive = Z.toArchive bs 
                    liftIO $ Z.extractFilesFromArchive [Z.OptDestination outPath] archive
                    
        goodHeader :: Integer -> Option a 
        goodHeader fileSize = 
            if fileSize > 0 then 
                header "range" ("bytes=" <> fromString (show fileSize) <> "-") 
            else mempty

hxInfos :: ServerInfo -> ProjectName -> IO (Either T.Text ProjectInfos)
hxInfos remote project = do 
    let remoteUrl = remotingUrl remote
    runReq defaultHttpConfig $ eitherToReq remoteUrl 
    where 
        request :: Url a -> Req (Either T.Text ProjectInfos)
        request url =
            reqBr GET url NoReqBody (("__x" =: serialize [HArray [HString "api", HString "infos"], HArray [HString $ unProjectName project]]) <> header "X-Haxe-Remoting" "1") $ \r -> do
                res <- B.drop 3 . B.concat <$> HC.brConsume (HC.responseBody r)
                let maybeStruct = parse deserialize "my attic" (T.pack . BC.unpack $ res)
                case maybeStruct of 
                    Right x@[HStructure _]-> 
                        pure $ hxDeserialize x 
                    Right x -> 
                        pure $ Left $ T.pack $ show x
                    Left e -> 
                        pure $ Left $ T.pack $ errorBundlePretty e
        eitherToReq :: Either (Url a) (Url b) -> Req (Either T.Text ProjectInfos)
        eitherToReq = \case 
            Left a -> request a
            Right a -> request a

prettyHaxeInfo :: ProjectInfos -> T.Text
prettyHaxeInfo infos@ProjectInfos{..} = 
    "Name: " <> pjname <> "\n" 
    <> "Tags: " <> T.intercalate ", " pjtags <> "\n"
    <> "Desc: " <> pjdescription <> "\n"
    <> "Website: " <> pjwebsite <> "\n"
    <> "License: " <> pjlicense <> "\n"
    <> "Owner: " <> pjowner <> "\n"
    <> "Version: " <> maybe "(no version released)" prettySemVer (getLatest infos) <> "\n" 
    <> "Releases: \n" <> T.intercalate "\n" (map (\VersionInfo{..} -> vdate <> " " <> prettySemVer vname <> " : " <> vcomments) pjversions) <> "\n"

fromRightError :: Show e => Either e b -> b 
fromRightError (Left e) = error (show e)
fromRightError (Right b) = b
safeName :: T.Text -> T.Text
safeName name = 
    if T.all (\x -> isDigit x || isLetter x || x == '_' || x == '.' || x == '-') name then 
        T.map (\x -> if x == '.' then ',' else x) name 
    else 
        error $ "Invalid parameter " ++ T.unpack name
hxDownloadHaxelib :: ServerInfo -> ProjectName -> SemVer -> String -> IO () 
hxDownloadHaxelib sinfos name ver = 
    let 
        filename = safeName (unProjectName name) <> "-" <> safeName (prettySemVer ver) <> ".zip" 
    in 
        hxDownload sinfos filename
