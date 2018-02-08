{-# LANGUAGE OverloadedStrings #-}
module Types.Name where

import Data.CaseInsensitive (CI)
import Data.Csv (FromField(..))
import qualified Data.CaseInsensitive as CI
import Data.Aeson (ToJSON(..), Value(..), FromJSON(..))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist.Class (PersistField(..))
import Database.Persist.Types (PersistValue(..))

newtype Name = Name { unName :: CI Text } deriving (Eq)

mkName :: Text -> Name
mkName = Name . CI.mk

unMask :: Name -> Text
unMask = CI.original . unName

instance PersistField Name where
    toPersistValue (Name name) = (PersistDbSpecific . encodeUtf8 . CI.original) name
    fromPersistValue (PersistDbSpecific bytestring) = (Right . Name . CI.mk . decodeUtf8) bytestring
    fromPersistValue _ = Left "Unable to extract Name"

instance ToJSON Name where
    toJSON = String . unMask

instance FromJSON Name where
    parseJSON (String name) = pure $ mkName name
    parseJSON invalid = typeMismatch "Name" invalid

instance Show Name where
    show = unpack . unMask

instance FromField Name where
    parseField s = pure $ (mkName . decodeUtf8) s
