{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.ServerEvent where

import Data.Aeson (ToJSON(..), FromJSON(..),Value, (.=), object, encode, decode, Encoding, defaultOptions, genericToEncoding)
import GHC.Generics
import Data.Text (Text)
import Model (Category, Expense)
import Types.Name (Name, unMask)
import Network.WebSockets (WebSocketsData(..), DataMessage(..))

data ServerEvent =
    NewCategoryCount Category Int
  | NoSuchCategory Name
  | NewExpense Expense
  | Empty deriving Generic

instance WebSocketsData ServerEvent where
    fromDataMessage (Text bs _) = maybe Empty id (decode bs)
    fromDataMessage (Binary _)  = Empty
    fromLazyByteString bs = maybe Empty id (decode bs)
    toLazyByteString = encode

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
    toJSON Empty                = let event = ("empty" :: Text)
                                      payload = ("none" :: Text)
                                   in object ["event" .= event, "payload" .= payload]
    toEncoding :: ServerEvent -> Encoding
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerEvent
