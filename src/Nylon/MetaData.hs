module Nylon.MetaData where 

import Data.Text qualified as T
import Nylon.Serializer
import Nylon.SemVer
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
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
data User = User
    { uname :: T.Text
    , ufullName :: T.Text }
    deriving (Eq, Show)
data FullUser = FullUser 
    {funame :: T.Text
    ,fufullName :: T.Text
    ,fuemail :: T.Text
    , fuprojects :: [T.Text] }
    deriving (Eq, Show)

instance HxDeserialize User where 
    hxDeserialize [HStructure fields] = 
        let 
            (HString name) = fields HM.! "name" 
            (HString fullName) = fields HM.! "fullname" 
        in 
            User name fullName
    hxDeserialize _ = error "Invalid User"
instance HxDeserialize VersionInfo where 
    hxDeserialize [HStructure fields] = 
        let 
            (HString date) = fields HM.! "date" 
            name = hxDeserialize [fields HM.! "name"]
            (HInt downloads) = fields HM.! "downloads"
            (HString comments) = fields HM.! "comments"
        in 
            VersionInfo date name downloads comments
    hxDeserialize _ = error "bad version info"

instance HxDeserialize ProjectInfos where 
    hxDeserialize [HStructure fields] = 
        let 
            (HString name) = fields HM.! "name" 
            (HString desc) = fields HM.! "desc"
            (HString web)  = fields HM.! "website"
            (HString owner)= fields HM.! "owner"
            (HArray contributors') = fields HM.! "contributors"
            contributors = map (hxDeserialize @User . L.singleton) contributors'
            (HString license) = fields HM.! "license"
            curver  = fromNullable (fields HM.! "curversion") >>= (\(HString x) -> Just x)
            (HInt downloads) = fields HM.! "downloads"
            (HArray versions') = fields HM.! "versions"
            versions = map (hxDeserialize @VersionInfo . L.singleton) versions'
            (HList tags') = fields HM.! "tags"
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

    
