{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ExpenseFile where

import Import
import Handler.Expense (expenseFileForm)
import Data.Csv (decodeByName, FromNamedRecord(..))
import qualified Data.Csv as Csv

postExpenseFileR :: Handler Html
postExpenseFileR = do
    ((result, _), _) <- runFormPost expenseFileForm
    case result of
        FormSuccess fileInfo -> handleFormSuccess fileInfo
        _ -> handleFormUnsuccess

-- vExpenseShell :: Either String (Vector ExpenseShell)
handleFormSuccess :: FileInfo -> Handler Html
handleFormSuccess fileInfo = do
    vExpenseShell <- runConduit $ fileSource fileInfo .| parseCSV
    case vExpenseShell of
        Left _ -> do
            setMessage "Failed to read CSV file. Contact administrator."
            redirect ExpenseR
        Right shells -> do
            -- process shells
            redirect ExpenseR

processShell :: TChan Text -> [CategoryId] -> UserId -> ExpenseShell -> IO ()
processShell tChan categoryIds userId ExpenseShell{..} = do
    let mCategoryId = findCategoryId categoryIds esCategory
    case mCategoryId of
        Nothing -> informNoSuchCategory tChan (NoSuchCategory esCategory)
        Just categoryId -> error "still working on the good case"

informNoSuchCategory :: TChan Text -> ServerEvent -> IO ()
informNoSuchCategory tChan serverEvent = atomically $ writeTChan tChan (toText serverEvent)


findCategoryId :: [CategoryId] -> Name -> Maybe CategoryId
findCategoryId = error "working on it"
-- how do we process a shell, what data do we need?
-- well, first we are going to insert the shell and then tell the tchan
-- in order to insert the shell we need an expense
-- in order to get an expense we need extra info no in the shell
-- we need a userid, a list of category ids and a utctime
-- we can get all of those and then create 
-- to get a concurrently value, we need an IO action
-- to get an IO action we need an IO action;
-- atomically can give us an IO action provided we give it an STM value
-- why can we leave the atomically part until teh end? I think we can

-- we want to do stuff concurrently, so for each shell, we want to create
-- a Concurrently value; then we want to monoidally concat all those values to
-- get one corrently, then we want to runconcurrently all of that
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
