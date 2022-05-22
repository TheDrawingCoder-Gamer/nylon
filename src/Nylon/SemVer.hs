{-# LANGUAGE TypeApplications, GADTs, OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Nylon.SemVer where 

import Data.Char (isDigit, toLower)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.String (IsString)
import Data.Aeson (FromJSON(..), (.:), withText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Nylon.Serializer
import Data.Maybe (fromJust)
import Data.Bifunctor qualified as BFu
data Preview 
    = Alpha
    | Beta
    | RC
    deriving (Eq, Ord, Enum, Bounded, Show)
data SemVer = SemVer 
    { semMajor :: Int
    , semMinor :: Int
    , semPatch :: Int
    , semPreview :: Maybe Preview
    , previewNum :: Maybe Int }
    deriving (Eq, Show, Ord)
type Parser = Parsec Void Text

instance FromJSON SemVer where 
    parseJSON = withText "SemVer" $ \v -> case parse parseSemVer "-" v of  
                                                Left e -> 
                                                    fail $ errorBundlePretty (e :: ParseErrorBundle Text Void)  
                                                Right p -> 
                                                    pure p
parseSemVer :: Parser SemVer 
parseSemVer = do
    major <- parserSaneNumber
    char '.'
    minor <- parserSaneNumber
    char '.' 
    patch <- parserSaneNumber
    option (SemVer major minor patch Nothing Nothing) $ do 
        char '-'
        preview <- parserPreview 
        option (SemVer major minor patch (Just preview) Nothing) $ do
            char '.'
            SemVer major minor patch (Just preview) . Just 
                <$> parserSaneNumber


parserPreview :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m Preview 
parserPreview = choice 
    [ chunk "alpha" >> pure Alpha
    , chunk "beta"  >> pure Beta 
    , chunk "rc"    >> pure RC ] 
parserSaneNumber :: (MonadParsec e s m, Token s ~ Char) => m Int 
parserSaneNumber = choice 
    [ char '0' >> pure 0 
    , read @Int <$> do
        first <- satisfy (\x -> x /= '0' && isDigit x)
        rest <- many digitChar 
        pure (first:rest)] <?> "a natural number, including zero"
prettySemVer :: SemVer -> T.Text
prettySemVer SemVer{..} = 
    T.pack (show semMajor) <> "." <> T.pack (show semMinor) <> "." <> T.pack (show semPatch) <>
        case semPreview of 
            Just s -> 
                "-" <> T.toLower (T.pack (show s)) <> 
                    case previewNum of 
                        Just n -> 
                            "." <> T.pack (show n) 
                        Nothing -> ""
            Nothing -> ""
instance HxDeserialize SemVer where 
    hxDeserialize [HString s] = 
        BFu.first (T.pack . errorBundlePretty) (parse parseSemVer "my attic" s)
    hxDeserialize _ = Left "invalid semver" 
