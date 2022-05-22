{-# LANGUAGE DeriveGeneric #-}
module Nylon.Serializer (
    HaxeValue(..) 
    , serialize 
    , deserialize 
    , fromNullable
    , HxDeserialize(..)
    , HxSerialize(..)) where 

import Polysemy qualified as P
import Polysemy.State qualified as PS
import Data.Map qualified as M
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.ByteString qualified as B
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.List (groupBy)
import Data.List qualified as L
import Data.ByteString.Base64 qualified as B64
import Data.Char (isDigit)
import Data.Void
import Data.Functor (($>), (<&>))
import Network.URI.Encode
import Data.Foldable (fold, foldlM)

data HaxeValue = 
    HNull 
    | HInt Int
    | HFloat Float
    | HBool Bool
    | HString T.Text
    | HList [HaxeValue]
    | HArray [HaxeValue] -- TODO: what seperates these? Haxe lists
    | HDate T.Text -- TODO: Actually parse date
    | HStructure [(HaxeValue, HaxeValue)]
    | HStringMap [(HaxeValue, HaxeValue)]
    | HIntMap    [(Int, HaxeValue)] -- TODO: Strongly type haxe maps
    | HObjectMap [(HaxeValue, HaxeValue)]
    | HBytes B.ByteString
    | HException HaxeValue
    | HClass HaxeValue [(HaxeValue, HaxeValue)]
    | HEnumName HaxeValue HaxeValue [HaxeValue]
    | HEnumIndex HaxeValue Int [HaxeValue]
    | HCustom HaxeValue T.Text
    | HStrCache Int
    | HCache Int
    | HVerbatim T.Text -- For internal use only
    deriving (Generic, Eq, Show)
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
    HString s -> let encoded  = encodeText s in "y" <> T.pack (show (T.length encoded)) <> ":" <> encoded
    HList i -> let items = T.concat $ map serializeItem i in "l" <> items <> "h"
    HArray i -> let items =  T.concat $ map serializeItem $ doNullMagic i in "a" <> items <> "h"
    HDate s -> "v" <> s
    HStructure i -> "o" <> L.foldl' (\a (k, v) -> a <> serializeItem k <> serializeItem v) "" i <> "g"
    HStringMap i -> "b" <> L.foldl' (\a (k, v) -> a <> serializeItem k <> serializeItem v) "" i <> "h"
    HIntMap    i -> "q" <> L.foldl' (\a (k, v) -> a <> ":" <> T.pack (show k) <> serializeItem v) "" i <> "h"
    HObjectMap i -> "M" <> L.foldl' (\a (k, v) -> a <> serializeItem k <> serializeItem v) "" i <> "h"
    HBytes b -> let encoded = hxBase64encode b in "s" <> T.pack (show (T.length encoded)) <> ":" <> encoded
    HException x -> "x" <> serializeItem x
    HClass name fields -> "c" <> serializeItem name <> L.foldl' (\a (k, v) -> a <> serializeItem k <> serializeItem v) "" fields <> "g"
    HEnumName enumName constructor args -> "w" <> serializeItem enumName <> serializeItem constructor <> ":" <> T.pack (show (length args)) <> T.concat (map serializeItem args)
    HEnumIndex enumName idx args -> "j" <> serializeItem enumName <> ":" <> T.pack (show idx) <> ":" <> T.pack (show (length args)) <> T.concat (map serializeItem args)
    HCustom name dta -> "C" <> serializeItem name <> dta <> "g"
    HVerbatim a -> a
    HStrCache a -> "R" <> T.pack (show a)
    HCache a -> "r" <> T.pack (show a)

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

deserialize :: Parsec Void T.Text [HaxeValue]
deserialize = do 
    many deserializeItem <&> uncache
deserializeItem :: Parsec Void T.Text HaxeValue
deserializeItem = do 
    choice 
        [ char 'n' $> HNull
        , char 'z' $> HInt 0
        , char 'i' *> parseInt <&> HInt
        , char 'k' $> HFloat (0/0)
        , char 'm' $> HFloat (-1/0)
        , char 'p' $> HFloat (1/0)
        , char 'd' *> parseFloat <&> HFloat
        , char 't' $> HBool True
        , char 'f' $> HBool False
        , char 'y' *> parseString <&> HString
        , char 'o' *> manyTill parseKeyValue (char 'g') <&> HStructure
        , char 'l' *> (manyTill deserializeItem (char 'h') <&> HList)
        , char 'a' *> ((concat <$> manyTill deserializeArrayItem (char 'h')) <&> HArray)
        , char 'v' *> fail "i'm tired"
        , char 'b' *> (manyTill  parseKeyValue (char 'h') <&> HStringMap) 
        , char 'q' *> (manyTill parseIntValue (char 'h') <&> HIntMap   )
        , char 'M' *> (manyTill parseKeyValue (char 'h') <&> HObjectMap)
        , char 's' *> parseBytes <&> HBytes
        , char 'x' *> deserializeItem <&> HException
        , char 'c' *> parseClass
        , char 'w' *> fail "Enums are unimplemented"
        , char 'j' *> fail "Enums are unimplemented"
        , char 'C' *> fail "Custom data is impossible to parse"
        , char 'R' *> parseInt <&> HStrCache
        , char 'r' *> parseInt <&> HCache
        ] 
deserializeArrayItem =
    choice 
        [ L.singleton <$> deserializeItem
        , char 'u' *> parseInt <&> flip replicate HNull ]
uncache :: [HaxeValue] -> [HaxeValue]
uncache input = 
    map uncacheItem values
    where
        (values, scache, cache) = genCache input
        uncacheItem = \case 
            HStrCache r -> HString $ scache !! r 
            HCache r -> cache !! r
            HArray arr -> HArray $ map uncacheItem arr
            HList arr -> HList $ map uncacheItem arr
            HStructure m -> HStructure $ uncacheMap m
            HStringMap m -> HStringMap $ uncacheMap m
            HIntMap m -> HIntMap $ reverse $ L.foldl' (\a (k, v) -> (k, uncacheItem v):a) [] m
            HObjectMap m -> HObjectMap $ uncacheMap m
            HClass n m -> HClass (uncacheItem n) $ uncacheMap m
            HEnumName n c a -> HEnumName (uncacheItem n) (uncacheItem c) (map uncacheItem a)
            HEnumIndex n i a -> HEnumIndex (uncacheItem n) i (map uncacheItem a)
            HCustom n c -> HCustom (uncacheItem n) c
            u -> u
        uncacheMap = 
            reverse . L.foldl' (\a (k, v) -> (uncacheItem k, uncacheItem v):a) []

type CacheList = ([HaxeValue], [T.Text], [HaxeValue])
type CacheItem = (HaxeValue, [T.Text], [HaxeValue])
genCache :: [HaxeValue] -> CacheList
genCache = P.run . PS.evalState (HNull, [], []) . genCacheSem
genCacheSem :: [HaxeValue] -> P.Sem (PS.State CacheItem ': r) CacheList
genCacheSem = do
    foldlM (\a i -> do
        (h, s, c) <- genCacheItem i
        pure $ a <> ([h], s, c)) ([],[],[])

genCacheItem :: HaxeValue -> P.Sem (PS.State CacheItem ': r) CacheItem
genCacheItem = \case  
    HString s -> pure (HString s, [s], [])
    ha@(HArray arr) -> do 
        let folder a l = (a <>) <$> genCacheItem l
        foldlM folder (ha, [], [ha]) arr
    o@(HStructure strct) -> genCacheNameStruct strct HStructure
    c@(HClass name strct) -> genCacheNameStruct strct (HClass name)
    j@(HEnumName (HString name) (HString constr) args) -> do
        stuff <- mapM genCacheItem args
        pure (j, [name, constr], concatMap third stuff)
    j@(HEnumIndex (HString name) idx args) -> do
        stuff <- mapM genCacheItem args
        pure (j, [name, name <> T.pack (show idx)], concatMap third stuff)
    l@(HList ls) -> foldlM (\a l -> (a <>) <$> genCacheItem l) (l, [], [l]) ls
    b@(HStringMap m) -> genCacheNameStruct m HStringMap
    q@(HIntMap m) -> foldlM (\a (_, l) -> (a <>) <$> genCacheItem l) (q, [], [q]) m
    m@(HObjectMap mp) -> genCacheNameStruct mp HObjectMap
    v@(HDate _) -> pure (v, [], [v])
    s@(HBytes _) -> pure (s, [], [s])
    -- default
    u -> pure (u, [], [])
    where 
        third (_, _, x) = x

genCacheNameStruct :: [(HaxeValue, HaxeValue)] -> ([(HaxeValue, HaxeValue)] -> HaxeValue) -> P.Sem (PS.State CacheItem ': r ) CacheItem
genCacheNameStruct thingies constr = 
     let (keys, values) = unzip thingies in 
        do 
            (_, keys', values') <- genHelper
            pure (constr thingies, keys', values' ) 
    where 
        second3 (_, x, _) = x
        third (_, _, x) = x
        genHelper = foldlM (\a (k, v) -> 
            do 
                cachedK <- genCacheItem k
                cachedV <- genCacheItem v
                pure $ a <> cachedK <> cachedV) (constr thingies, [], [constr thingies]) thingies

-- to satisfy my mental state. NOT FOR ACTUAL USE!
instance Semigroup HaxeValue where 
    i <> _ = i
parseInt :: Parsec Void T.Text Int 
parseInt = do 
    sign <- optional $ char '-' 
    int <- some (satisfy isDigit) 
    case sign of 
        Just _ -> pure $ read int * (-1) 
        Nothing -> pure $ read int
parseFloat :: Parsec Void T.Text Float 
parseFloat = do 
    float <- some (satisfy (\c -> isDigit c || c == '.' || c == '+' || c == '-' || c == ',')) 
    pure $ read float
parseString :: Parsec Void T.Text T.Text 
parseString = do 
    num <- read <$> some (satisfy isDigit) 
    char ':'
    decodeText . T.pack <$> count num anySingle 

parseKeyValue :: Parsec Void T.Text (HaxeValue, HaxeValue)
parseKeyValue = (,)
    <$> deserializeItem 
    <*> deserializeItem
parseNameValue :: Parsec Void T.Text (T.Text, HaxeValue)
parseNameValue = do 
    (HString key, value) <- parseKeyValue
    pure (key, value)
parseIntValue :: Parsec Void T.Text (Int, HaxeValue) 
parseIntValue = do 
    char ':'
    num <- read <$> some (satisfy isDigit)
    item <- deserializeItem 
    pure (num, item)
parseBytes :: Parsec Void T.Text B.ByteString 
parseBytes = do 
    c <- read <$> some (satisfy isDigit)
    char ':' 
    chars <- T.pack <$> count c anySingle
    case hxBase64decode chars of 
        Left e -> fail $ T.unpack e 
        Right a -> pure a
parseClass :: Parsec Void T.Text HaxeValue 
parseClass = HClass 
    <$> deserializeItem
    <*> manyTill parseKeyValue (char 'g')
hxBase64encode :: B.ByteString -> T.Text
hxBase64encode = T.replace "+" "%" . T.replace "/" ":" . B64.encodeBase64
hxBase64decode :: T.Text -> Either T.Text B.ByteString
hxBase64decode = B64.decodeBase64 . TE.encodeUtf8 . T.replace "%" "+" . T.replace ":" "/"
parseCustom :: Parsec Void T.Text HaxeValue  
parseCustom = HCustom
    <$> deserializeItem 
    <*> (T.pack <$> manyTill anySingle (single 'g'))
    

fromNullable :: HaxeValue -> Maybe HaxeValue 
fromNullable HNull = Nothing
fromNullable x = Just x

class HxSerialize x where 
    hxSerialize :: x -> Either T.Text [HaxeValue]
class HxDeserialize x where 
    hxDeserialize :: [HaxeValue] -> Either T.Text x

