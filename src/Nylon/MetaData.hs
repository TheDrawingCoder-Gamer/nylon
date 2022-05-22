{-# LANGUAGE RecordWildCards #-}
module Nylon.MetaData where 

import Data.Text qualified as T
import Nylon.Serializer
import Nylon.SemVer
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Functor ((<&>))
import Debug.Trace
import Data.Ord (Down(..))
import Data.Maybe (isNothing, isJust)
import Polysemy qualified as P
import Polysemy.Error qualified as P
import Polysemy.Fail qualified as P
data ProjectInfos = ProjectInfos
    { pjname        :: T.Text
    , pjdescription :: T.Text
    , pjwebsite     :: T.Text
    , pjowner       :: T.Text
    , pjcontributors :: [User]
    , pjlicense      :: T.Text
    , pjversion      :: Maybe T.Text
    , pjdownloads    :: Int 
    , pjtags         :: [T.Text] 
    , pjversions     :: [VersionInfo] }
    deriving (Eq, Show)
data VersionInfo = VersionInfo 
    { vdate :: T.Text
    , vname :: SemVer
    , vdownloads :: Int 
    , vcomments :: T.Text} 
    deriving (Eq, Show)
instance Ord VersionInfo where 
    compare VersionInfo{vname=v1} VersionInfo{vname=v2} = compare v1 v2
data User = User
    { uname :: Maybe T.Text
    , ufullName :: Maybe T.Text }
    deriving (Eq, Show)
data FullUser = FullUser 
    {funame :: T.Text
    ,fufullName :: T.Text
    ,fuemail :: T.Text
    , fuprojects :: [T.Text] }
    deriving (Eq, Show)
getLatestWith :: (SemVer -> Bool) -> ProjectInfos -> Maybe SemVer 
getLatestWith doPreview ProjectInfos{..} 
    | null pjversions = Nothing
    | otherwise = 
       vname <$> headMaybe (dropWhile (not . doPreview . vname) $ map getDown (L.sort (map Down pjversions)))
getLatest :: ProjectInfos -> Maybe SemVer 
getLatest = getLatestWith (isNothing . semPreview)
headMaybe ls
    | null ls = Nothing
    | otherwise = Just $ head ls
    
instance HxDeserialize User where 
    hxDeserialize [HStructure fields'] = 
        let 
            fields = HM.fromList fields'
            name = (fields HM.!? HString "name") >>= fromNullable >>= (\(HString x) -> Just x) 
            fullName = fields HM.!? HString "fullname" >>= fromNullable >>= (\(HString x) -> Just x)
        in
            -- Given user has no required fields, it will always succeed
            Right $ User name fullName
    hxDeserialize _ = Left "Invalid User"
instance HxDeserialize VersionInfo where 
    hxDeserialize [HStructure fields'] = 
        P.run $ P.runError $ P.failToError T.pack polysemyHelp
        where 
            polysemyHelp :: P.Sem [P.Fail, P.Error T.Text] VersionInfo 
            polysemyHelp = do 
                let fields = HM.fromList fields' 
                (HString date) <- P.note "Missing required field (date)" (fields HM.!? HString "date") 
                nameField <- P.note "Missing required field (name)" (fields HM.!? HString "name")
                name <- P.fromEither $ hxDeserialize [nameField]
                (HInt downloads) <- P.note "Missing required field (downloads)" (fields HM.!? HString "downloads")
                (HString comments) <- P.note "Missing required field (comments)" (fields HM.!? HString "comments") 
                pure $ VersionInfo date name downloads comments
    hxDeserialize _ = Left "bad version info"

instance HxDeserialize ProjectInfos where 
    hxDeserialize [HStructure fields'] = 
        P.run $ P.runError $ P.failToError T.pack helper
        where 
            helper :: P.Sem [P.Fail, P.Error T.Text] ProjectInfos
            helper = do 
                let fields = HM.fromList fields' 
                (HString name) <- P.note "Missing required field (name)" (fields HM.!? HString "name") 
                (HString desc) <- P.note "Missing required field (desc)" (fields HM.!? HString "desc")
                (HString web ) <- P.note "Missing required field (website)" (fields HM.!? HString "website")
                (HString owner) <- P.note "Missing required field (owner)" (fields HM.!? HString "owner")  
                (HArray contributors') <- P.note "Missing required field (contributors)" (fields HM.!? HString "contributors")
                contributors <- mapM (P.fromEither . hxDeserialize @User . L.singleton) contributors'
                (HString license) <- P.note "Missing required field (license)" (fields HM.!? HString "license")
                let curver = fields HM.!? HString "curversion" >>= fromNullable >>= (\(HString x) -> Just x)
                (HInt downloads) <- P.note "Missing required field (downloads)" (fields HM.!? HString "downloads")
                (HArray versions') <- P.note "Missing required field (versions)" (fields HM.!? HString "versions")
                versions <- mapM (P.fromEither . hxDeserialize @VersionInfo . L.singleton) versions'
                (HList tags') <- P.note "Missing required field (tags)" (fields HM.!? HString "tags") 
                let tags = map (\(HString s) -> s) tags'
                pure $ ProjectInfos
                    name
                    desc
                    web
                    owner
                    contributors
                    license
                    curver
                    downloads
                    tags
                    versions



    hxDeserialize _ = 
        error "invalid ProjectInfos"

 
