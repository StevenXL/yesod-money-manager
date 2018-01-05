{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings #-}
module Fields.NameField (nameField) where

import Import hiding (fieldParse, fieldView, fieldEnctype)
import Types.Name (mkName, unMask)

nameField :: Field Handler Name
nameField = Field fieldParse fieldView fieldEnctype

fieldParse :: [Text] -> b -> Handler (Either (SomeMessage (HandlerSite Handler)) (Maybe Name))
fieldParse [name] _ = (pure . Right . Just . mkName) name
fieldParse [] _     = pure $ Right Nothing
fieldParse _ _      = pure $ Left "Only one value is acceptable"

fieldView :: FieldViewFunc Handler Name
fieldView idAttr nameAttr otherAttrs eResult isReq =
        [whamlet|
            <input id="#{idAttr}" name="#{nameAttr}" *{otherAttrs} type="text" value="#{either id unMask eResult}" :isReq:required>
        |]

fieldEnctype :: Enctype
fieldEnctype = UrlEncoded
