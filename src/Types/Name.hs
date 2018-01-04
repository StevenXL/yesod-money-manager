{-# LANGUAGE OverloadedStrings #-}
module Types.Name where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Aeson (ToJSON(..), Value(..), FromJSON(..))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist.Class (PersistField(..))
import Database.Persist.Types (PersistValue(..))

newtype Name = Name { unName :: CI Text } deriving (Eq, Show)

instance PersistField Name where
    toPersistValue (Name name) = (PersistDbSpecific . encodeUtf8 . CI.original) name
    fromPersistValue (PersistDbSpecific bytestring) = (Right . Name . CI.mk . decodeUtf8) bytestring
    fromPersistValue _ = Left "Unable to extract Name"

instance ToJSON Name where
    toJSON (Name name) = (String . CI.original) name

instance FromJSON Name where
    parseJSON (String name) = (pure . Name . CI.mk) name
    parseJSON invalid = typeMismatch "Name" invalid
