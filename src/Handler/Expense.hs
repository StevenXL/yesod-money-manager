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
    userId <- entityKey <$> requireAuth
    (formWidget, formEnctype) <- generateFormPost (expenseForm userId categories)
    defaultLayout $(widgetFile "expense")


postExpenseR :: Handler Html
postExpenseR = do
    categories <- runDB $ selectList [] [Asc CategoryName]
    userId <- entityKey <$> requireAuth
    ((result, formWidget), formEnctype) <- runFormPost (expenseForm userId categories)
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

expenseForm :: UserId -> [Entity Category] -> Form Expense
expenseForm userId = renderBootstrap3 BootstrapBasicForm . expenseAForm  userId . toOptions

type CategoryOption = (Text, Key Category)

toOptions :: [Entity Category] -> [CategoryOption]
toOptions = map categoryToOption

categoryToOption :: Entity Category -> CategoryOption
categoryToOption category = (categoryToText category, entityKey category)
    where categoryToText = CI.original . unName . categoryName . entityVal

expenseAForm :: UserId -> [CategoryOption] -> AForm Handler Expense
expenseAForm userId categoryOptions = Expense
    <$> areq intField (bfs ("Amount" :: Text)) Nothing
    <*> areq textField (bfs ("Item" :: Text))  Nothing
    <*> areq textField (bfs ("Vendor" :: Text))  Nothing
    <*> areq (selectFieldList categoryOptions) (bfs ("Category" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> pure userId
