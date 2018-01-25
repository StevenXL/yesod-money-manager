{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Expense where

import Import
import Data.Aeson (encode)
import qualified Data.CaseInsensitive as CI
import Database.Esqueleto ((^.), InnerJoin(..), unValue)
import qualified Database.Esqueleto as E
import qualified Prelude as P

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
                    <form method=post action=@{ExpenseR} enctype=#{formEnctype}>
                        ^{formWidget}
                        <button>Submit
                |]

handleFormSuccess :: Expense -> Handler Html
handleFormSuccess expense = do
    _ <- runDB $ insert expense
    wChan <- categoryCountWriteChan <$> getYesod
    Just category <- runDB $ get (expenseCategoryId expense)
    expensesCount :: [E.Value Int] <- runDB $ E.select $ E.from $ \(c `InnerJoin` e) -> do
        E.on (e ^. ExpenseCategoryId E.==. c ^. CategoryId)
        E.groupBy (c ^. CategoryId)
        E.where_ (c ^. CategoryId E.==. E.val (expenseCategoryId expense))
        let eCount = E.count (e ^. ExpenseId)
        return eCount
    let expenseCount = if null expensesCount then 0 else (unValue . P.head) expensesCount
        serverEvent = NewCategoryCount category expenseCount
    atomically (writeToChan wChan serverEvent)
    setMessage $ toHtml ("Successfully created expense." :: String)
    redirect ExpenseR

writeToChan :: TChan Text -> ServerEvent -> STM ()
writeToChan wChan serverEvent = do
        writeTChan wChan $ (decodeUtf8 . toStrict . encode) serverEvent

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
