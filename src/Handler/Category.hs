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
    allCategories <- runDB $ selectList [] [] :: Handler [Entity Category]
    (formWidget, formEnctype) <- generateFormPost categoryForm
    defaultLayout $(widgetFile "category")

postCategoryR :: Handler Html
postCategoryR = do
    allCategories <- runDB $ selectList [] [] :: Handler [Entity Category]
    ((result, formWidget), formEnctype) <- runFormPost categoryForm
    case result of
        FormSuccess category -> handleFormSuccess category
        _ -> defaultLayout $(widgetFile "category")

categoryAForm :: AForm Handler Category
categoryAForm = Category
    <$> areq validatedNameField (bfs ("Name" :: Text)) Nothing
    where validatedNameField = checkM ensureNoDuplicates nameField

categoryForm :: Form Category
categoryForm = renderBootstrap3 BootstrapBasicForm categoryAForm

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
