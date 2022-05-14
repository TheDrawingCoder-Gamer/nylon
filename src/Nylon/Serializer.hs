{-# LANGUAGE DeriveGeneric #-}
module Nylon.Serializer where 

import Polysemy qualified as P
import Polysemy.State
import Data.Map qualified as M
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.ByteString qualified as B
import GHC.Generics (Generic)
import Text.Megaparsec
import Data.Text qualified as T
import Data.List (groupBy)
import Data.List qualified as L
import Data.ByteString.Base64 qualified as B64
data HaxeValue = 
    HNull 
    | HInt Int
    | HFloat Float
    | HBool Bool
    | HString T.Text
    | HList [HaxeValue]
    | HArray [HaxeValue] -- TODO: what seperates these? Haxe lists
    | HDate T.Text -- TODO: Actually parse date
    | HStructure (HM.HashMap T.Text HaxeValue)
    | HStringMap (M.Map T.Text HaxeValue)
    | HIntMap    (M.Map Int HaxeValue) -- TODO: Strongly type haxe maps
    | HObjectMap (HM.HashMap HaxeValue HaxeValue)
    | HBytes B.ByteString
    | HException T.Text
    | HClass T.Text (HM.HashMap T.Text HaxeValue)
    | HEnumName T.Text T.Text [HaxeValue]
    | HEnumIndex T.Text Int [HaxeValue]
    | HCustom T.Text T.Text
    | HVerbatim T.Text -- For internal use only
    deriving (Generic, Eq)
instance Hashable HaxeValue

serialize :: [HaxeValue] -> T.Text 
serialize =  T.concat . map serializeItem
    
serializeItem :: HaxeValue -> T.Text
serializeItem = \case 
    HNull -> "n"
    HInt 0 -> "z"
    HInt n -> "i" <> T.pack (show n)
    HFloat f | isNaN f -> "k"
    HFloat f | isInfinite f && f < 0 -> "m"
    HFloat f | isInfinite f -> "p"
    HFloat f -> "d" <> T.pack (show f)
    HBool True -> "t" 
    HBool False -> "f"
    HString s -> "y" <> T.pack (show (T.length s)) <> ":" <> s
    HList i -> let items = T.concat $ map serializeItem i in "l" <> items <> "h"
    HArray i -> let items =  T.concat $ map serializeItem $ doNullMagic i in "a" <> items <> "h"
    HDate s -> "v" <> s
    HStructure i -> "o" <> HM.foldlWithKey' (\a k v -> a <> serializeItem (HString k) <> serializeItem v) "" i <> "g"
    HStringMap i -> "b" <> M.foldlWithKey' (\a k v -> a <> serializeItem (HString k) <> serializeItem v) "" i <> "h"
    HIntMap    i -> "q" <> M.foldlWithKey' (\a k v -> a <> ":" <> T.pack (show k) <> serializeItem v) "" i <> "h"
    HObjectMap i -> "M" <> HM.foldlWithKey' (\a k v -> a <> serializeItem k <> serializeItem v) "" i <> "h"
    HBytes b -> let encoded = B64.encodeBase64 b in "s" <> T.pack (show (T.length encoded)) <> ":" <> encoded
    HException x -> "x" <> serializeItem (HString x)
    HClass name fields -> "c" <> serializeItem (HString name) <> HM.foldlWithKey' (\a k v -> a <> serializeItem (HString k) <> serializeItem v) "" fields <> "g"
    HEnumName enumName constructor args -> "w" <> serializeItem (HString enumName) <> serializeItem (HString constructor) <> ":" <> T.pack (show (length args)) <> T.concat (map serializeItem args)
    HEnumIndex enumName idx args -> "j" <> serializeItem (HString enumName) <> ":" <> T.pack (show idx) <> ":" <> T.pack (show (length args)) <> T.concat (map serializeItem args)
    HCustom name dta -> "C" <> serializeItem (HString name) <> dta <> "g"
    HVerbatim a -> a

serializeKeyValue :: HaxeValue -> HaxeValue -> T.Text
serializeKeyValue key value = 
    let 
        key' = serializeItem key
        value' = serializeItem value
    in 
        key' <> value'
doNullMagic :: [HaxeValue] -> [HaxeValue]
doNullMagic list = 
    let betterGroup = groupBy (\a b -> a == b && a == HNull) list in 
        concatMap replaceNulls betterGroup
    where
        
        replaceNulls [x] = [x]
        replaceNulls ls = 
            L.singleton $ HVerbatim $ "u" <> T.pack (show (length ls))
            
