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
    (formWidget, formEnctype) <- generateFormPost (expenseForm categories)
    defaultLayout $(widgetFile "expense")


postExpenseR :: Handler Html
postExpenseR = do
    categories <- runDB $ selectList [] [Asc CategoryName]
    ((result, formWidget), formEnctype) <- runFormPost (expenseForm categories)
    case result of
        FormSuccess expense -> handleFormSuccess expense
        _ -> defaultLayout $
                [whamlet|
                    <form method=post action=@{ExpenseR} enctype={formEnctype}>
                        ^{formWidget}
                        <button>Submit
                |]

handleFormSuccess :: Expense -> Handler Html
handleFormSuccess expense = do
    _ <- runDB $ insert expense
    setMessage $ toHtml ("Successfully created expense." :: String)
    redirect ExpenseR

expenseForm :: [Entity Category] -> Form Expense
expenseForm = renderBootstrap . expenseAForm . toOptions

type CategoryOption = (Text, Key Category)

toOptions :: [Entity Category] -> [CategoryOption]
toOptions = map categoryToOption

categoryToOption :: Entity Category -> CategoryOption
categoryToOption category = (categoryToText category, entityKey category)
    where categoryToText = CI.original . unName . categoryName . entityVal

expenseAForm :: [CategoryOption] -> AForm Handler Expense
expenseAForm categoryOptions = Expense
    <$> areq intField "Amount" Nothing
    <*> areq textField "Item"  Nothing
    <*> areq textField "Vendor"  Nothing
    <*> areq (selectFieldList categoryOptions) "Category" Nothing
    <*> lift (liftIO getCurrentTime)
