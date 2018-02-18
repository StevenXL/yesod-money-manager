{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ExpenseFile where

import Import
import Handler.Expense (expenseFileForm)
import Data.Csv (decodeByName, FromNamedRecord(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently_)
import Data.Pool (withResource)
import Database.Persist.Sql (ConnectionPool)
import qualified Data.Csv as Csv

postExpenseFileR :: Handler Html
postExpenseFileR = do
    ((result, _), _) <- runFormPost expenseFileForm
    case result of
        FormSuccess fileInfo -> handleFormSuccess fileInfo
        _ -> handleFormUnsuccess

handleFormSuccess :: FileInfo -> Handler Html
handleFormSuccess fileInfo = do
    vExpenseShell <- runConduit $ fileSource fileInfo .| parseCSV
    case vExpenseShell of
        Left _ -> do
            setMessage "Failed to read CSV file. Contact administrator."
            redirect ExpenseR
        Right shells -> do
            tChan      <- channel <$> getYesod
            categories <- runDB $ selectList [] []
            user       <- entityKey <$> requireAuth
            pool       <- appConnPool <$> getYesod
            let processWith = processShell tChan categories user pool
            _          <- liftIO $ forkIO $ mapConcurrently_ processWith shells
            redirect ExpenseR

processShell :: TChan Text -> [Entity Category] -> UserId -> ConnectionPool -> ExpenseShell -> IO ()
processShell tChan categories userId pool ExpenseShell{..} = do
    let mCategoryId = findCategoryId categories esCategory
    case mCategoryId of
        Nothing -> informNoSuchCategory tChan (NoSuchCategory esCategory)
        Just categoryId -> do
            utcTime <- getCurrentTime
            let expense = Expense esAmount esItem esVendor categoryId utcTime userId
            _       <- withResource pool (runReaderT $ insert_ expense)
            let serverEvent = NewExpense expense
            atomically $ writeTChan tChan (toText serverEvent)

informNoSuchCategory :: TChan Text -> ServerEvent -> IO ()
informNoSuchCategory tChan serverEvent = atomically $ writeTChan tChan (toText serverEvent)


findCategoryId :: [Entity Category] -> Name -> Maybe CategoryId
findCategoryId [] _ = Nothing
findCategoryId (ec:ecs) name = if categoryHasName
                                   then Just $ entityKey ec
                                   else findCategoryId ecs name
                                where categoryHasName = (categoryName . entityVal) ec == name

parseCSV :: Monad m => ConduitM ByteString o m (Either String (Vector ExpenseShell))
parseCSV = do
    mBS <- await
    case mBS of
        Just bs -> return $ pure snd <*> (decodeByName . fromStrict) bs
        Nothing -> return $ Left "No data found"

handleFormUnsuccess :: Handler Html
handleFormUnsuccess = do
    setMessage "Invalid File Form Submitted"
    redirect ExpenseR

-- PROBALY GOES IN ANOTHER MODULE ; ORGANIZE LATER

data ExpenseShell = ExpenseShell { esAmount :: !Int, esItem :: !Text, esVendor :: !Text, esCategory :: !Name } deriving (Eq, Show)

instance FromNamedRecord ExpenseShell where
    parseNamedRecord r =
        pure ExpenseShell         <*>
        r `Csv.lookup` "Amount"   <*>
        r `Csv.lookup` "Item"     <*>
        r `Csv.lookup` "Vendor"   <*>
        r `Csv.lookup` "Category"
