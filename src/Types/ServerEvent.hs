{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Types.ServerEvent where

import Data.Aeson (ToJSON(..), Value, (.=), object, encode)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Model (Category, Expense)
import Data.ByteString.Lazy (toStrict)
import Types.Name (Name, unMask)

data ServerEvent =
    NewCategoryCount Category Int
  | NoSuchCategory Name
  | NewExpense Expense

toText :: ServerEvent -> Text
toText = decodeUtf8 . toStrict . encode

instance ToJSON ServerEvent where
    toJSON :: ServerEvent -> Value
    toJSON (NewCategoryCount category count) = let event = ("newCategoryCount" :: Text)
                                                   payload = object ["category" .= category, "count" .= count]
                                               in object ["event" .= event, "payload" .= payload]
    toJSON (NoSuchCategory name) = let event = ("noSuchCategory" :: Text)
                                       payload = object ["name" .= unMask name]
                                   in object ["event" .= event, "payload" .= payload]
    toJSON (NewExpense expense) = let event = ("newExpense" :: Text)
                                      payload = toJSON expense
                                  in object ["event" .= event, "payload" .= payload]
