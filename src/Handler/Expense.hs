{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Expense where

import Import

getExpenseR :: Handler Html
getExpenseR = do
    categories <- runDB $ selectList [] [Asc CategoryName]
    let categoryOptions = toOptions categories
        rExpenseAForm = expenseAForm categoryOptions
    (formWidget, formEnctype) <- generateFormPost (renderBootstrap rExpenseAForm)
    defaultLayout $(widgetFile "expense")

type CategoryOption = (Text, Key Category)

expenseAForm :: [CategoryOption] -> AForm Handler Expense
expenseAForm categoryOptions = Expense
    <$> areq intField "Amount" Nothing
    <*> areq textField "Item"  Nothing
    <*> areq textField "Vendor"  Nothing
    <*> areq (selectFieldList categoryOptions) "Category" Nothing
    <*> lift (liftIO getCurrentTime)

toOptions :: [Entity Category] -> [CategoryOption]
toOptions categories = Prelude.map categoryToOption categories

categoryToOption :: Entity Category -> CategoryOption
categoryToOption category = (categoryName $ entityVal category, entityKey category)
