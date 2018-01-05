{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Expense where

import Import
import qualified Data.CaseInsensitive as CI

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
toOptions categories = map categoryToOption categories

categoryToOption :: Entity Category -> CategoryOption
categoryToOption category = (categoryToText category, entityKey category)
    where categoryToText = CI.original . unName . categoryName . entityVal
