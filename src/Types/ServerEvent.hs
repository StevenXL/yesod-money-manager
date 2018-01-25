{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Types.ServerEvent where

import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Text (Text)
import Model (Category)

data ServerEvent = NewCategoryCount Category Int

instance ToJSON ServerEvent where
    toJSON :: ServerEvent -> Value
    toJSON (NewCategoryCount category count) = let event = ("newCategoryCount" :: Text)
                                                   payload = object [ "category" .= category, "count" .= count]
                                               in object [ "event" .= event, "payload" .= payload ]
