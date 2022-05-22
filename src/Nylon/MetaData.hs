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
            User name fullName
    hxDeserialize _ = error "Invalid User"
instance HxDeserialize VersionInfo where 
    hxDeserialize [HStructure fields'] = 
        let 
            fields = HM.fromList fields'
            (HString date) = fields HM.! HString "date" 
            name = hxDeserialize [fields HM.! HString "name"]
            (HInt downloads) = fields HM.! HString "downloads"
            (HString comments) = fields HM.! HString "comments"
        in 
            VersionInfo date name downloads comments
    hxDeserialize _ = error "bad version info"

instance HxDeserialize ProjectInfos where 
    hxDeserialize [HStructure fields'] = 
        let 
            fields = HM.fromList fields'
            (HString name) = fields HM.! HString "name" 
            (HString desc) = fields HM.! HString "desc"
            (HString web)  = fields HM.! HString "website"
            (HString owner)= fields HM.! HString "owner"
            (HArray contributors') = fields HM.! HString "contributors"
            contributors = map (hxDeserialize @User . L.singleton) contributors'
            (HString license) = fields HM.! HString "license"
            curver  = fields HM.!? HString "curversion" >>= fromNullable >>= (\(HString x) -> Just x)
            (HInt downloads) = fields HM.! HString "downloads"
            (HArray versions') = fields HM.! HString "versions"
            versions = map (hxDeserialize @VersionInfo . L.singleton) versions'
            (HList tags') = fields HM.! HString "tags"
            tags = map (\(HString s) -> s) tags'
        in 
            ProjectInfos 
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

 
