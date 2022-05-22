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
import Data.Text (Text, unpack)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Binary qualified as CB
import Data.String
import Data.Bifunctor qualified as BF
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Network.HTTP.Client qualified as HC
import Text.Megaparsec (parse, errorBundlePretty)
hxDownload :: ServerInfo -> Text -> String ->  IO ()
hxDownload remote filename outPath = do 
    let fileUrl = BF.bimap addOtherThings addOtherThings $ siteUrl remote
    handle <- openFile outPath AppendMode
    fileSize <- hFileSize handle
    
    case fileUrl of 
        Left a -> 
            runReq defaultHttpConfig $ request (goodHeader fileSize) handle a
        Right a -> 
            runReq defaultHttpConfig $ request (goodHeader fileSize) handle a
    where 
        addOtherThings :: Url a -> Url a 
        addOtherThings = (/: repository) . (/: filename)
        request :: Option a -> Handle -> Url a -> Req () 
        request optionals handle url = 
            reqBr GET url NoReqBody optionals $ \r ->
                runConduitRes $ responseBodySource r .| CB.sinkHandle handle
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
                        pure $ Right $ hxDeserialize x 
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
    -- TODO: Pretty print instead of showing
    <> "Releases: \n" <> T.intercalate "\n" (map (\VersionInfo{..} -> vdate <> " " <> prettySemVer vname <> " : " <> vcomments) pjversions) <> "\n"


