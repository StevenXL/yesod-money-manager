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
        _ -> defaultLayout [whamlet|
                <form> method=post action=@{CategoryR} enctype={formEnctype}>
                    ^{formWidget}
                    <button>Submit
             |]

categoryAForm :: AForm Handler Category
categoryAForm = Category
    <$> areq nameField "Name" Nothing

categoryForm :: Form Category
categoryForm = renderBootstrap categoryAForm

handleFormSuccess :: Category -> Handler Html
handleFormSuccess category@(Category name) = do
    _ <- runDB $ insert category
    setMessage $ toHtml $ "Successfully created category " ++ show name
    redirect CategoryR

-- put the category in the db
-- redirect to the category index page
