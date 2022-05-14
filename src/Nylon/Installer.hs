module Nylon.Installer where 

import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Nylon.Data
import System.IO
import Data.Text (Text, unpack)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Binary qualified as CB
import Data.String
import Data.Bifunctor qualified as BF
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
        
