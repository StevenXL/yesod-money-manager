{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Category where

import Import
import Fields.NameField

-- display a form for creating a cateogry
getCategoryR :: Handler Html
getCategoryR = do
    (formWidget, formEnctype) <- generateFormPost categoryForm
    defaultLayout $(widgetFile "category")

postCategoryR :: Handler Html
postCategoryR = do
    ((result, formWidget), formEnctype) <- runFormPost categoryForm
    case result of
        FormSuccess category -> handleFormSuccess category
        _ -> defaultLayout $
                [whamlet|
                    <form method=post action=@{CategoryR} enctype={formEnctype}>
                        ^{formWidget}
                        <button>Submit
                |]

categoryAForm :: AForm Handler Category
categoryAForm = Category
    <$> areq validatedNameField "Name" Nothing
    where validatedNameField = checkM ensureNoDuplicates nameField

categoryForm :: Form Category
categoryForm = renderBootstrap categoryAForm

handleFormSuccess :: Category -> Handler Html
handleFormSuccess category@(Category name) = do
    _ <- runDB $ insert category
    setMessage $ toHtml $ "Successfully created category " ++ show name
    redirect CategoryR

ensureNoDuplicates :: Name -> Handler (Either Text Name)
ensureNoDuplicates name = do
    maybePerson <- runDB $ getBy (UniqueName name)
    return $ maybe (Right name) (const msg) maybePerson
    where msg = Left $ "Name " <> unMask name <> " is already taken."