{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Category where

import Import
import Fields.NameField
import Database.Esqueleto (SqlReadT, (^.), LeftOuterJoin(..), unValue)
import qualified Database.Esqueleto as E
import Yesod.WebSockets (WebSocketsT, webSockets, sendTextData, sendClose)

-- HANDLERS

getCategoryR :: Handler Html
getCategoryR = do
    webSockets categoryCountPublisher
    badges <- categoryBadges
    (formWidget, formEnctype) <- generateFormPost categoryForm
    defaultLayout $(widgetFile "category")

postCategoryR :: Handler Html
postCategoryR = do
    badges <- categoryBadges
    ((result, formWidget), formEnctype) <- runFormPost categoryForm
    case result of
        FormSuccess category -> handleFormSuccess category
        _ -> defaultLayout $(widgetFile "category")

-- what do we want our websockets app to do really?
-- Well, we want it to forever read from a channel
-- When data comes in to that channel, then we want to send a message to the
-- client
-- it publishes the count of a category
categoryCountPublisher :: WebSocketsT Handler ()
categoryCountPublisher = do
    sendTextData ("" :: Text)
    sendClose ("Thank you for coming." :: Text)

-- HANDLER HELPERS

handleFormSuccess :: Category -> Handler Html
handleFormSuccess category@(Category name) = do
    _ <- runDB $ insert category
    setMessage $ toHtml $ "Successfully created category " ++ show name
    redirect CategoryR

categoryBadges :: Handler [Badge]
categoryBadges = do
    categoriesWithExpenseCount <- runDB categoriesWithExpenseCountQuery
    return $ categoriesToBadges categoriesWithExpenseCount


-- FORMS

categoryAForm :: AForm Handler Category
categoryAForm = Category
    <$> areq validatedNameField (bfs ("Name" :: Text)) Nothing
    where validatedNameField = checkM ensureNoDuplicates nameField

categoryForm :: Form Category
categoryForm = renderBootstrap3 BootstrapBasicForm categoryAForm


ensureNoDuplicates :: Name -> Handler (Either Text Name)
ensureNoDuplicates name = do
    maybePerson <- runDB $ getBy (UniqueName name)
    return $ maybe (Right name) (const msg) maybePerson
    where msg = Left $ "Name " <> unMask name <> " is already taken."

-- QUERIES
categoriesWithExpenseCountQuery :: MonadIO m => SqlReadT m [(Entity Category, E.Value Int)]
categoriesWithExpenseCountQuery = E.select $ E.from $ \(c `LeftOuterJoin` e) -> do
    E.on (e ^. ExpenseCategoryId E.==. c ^. CategoryId)
    E.groupBy (c ^. CategoryId)
    let cr = E.count (e ^. ExpenseAmount)
    return (c, cr)

-- OTHER
categoriesToBadges :: [(Entity Category, E.Value Int)] -> [Badge]
categoriesToBadges = map categoryToBadge

categoryToBadge :: (Entity Category, E.Value Int) -> Badge
categoryToBadge (categoryEntity, expenseCount) = let eCount = unValue expenseCount
                                                     label = (unMask . categoryName . entityVal) categoryEntity
                                                 in Badge label eCount
