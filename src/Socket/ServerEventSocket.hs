{-# LANGUAGE NoImplicitPrelude #-}

module Socket.ServerEventSocket where

import Import
import Yesod.WebSockets (WebSocketsT, sendTextData)

serverEventSocket :: WebSocketsT Handler ()
serverEventSocket = do
    wChan <- channel <$> getYesod
    rChan <- atomically $ dupTChan wChan
    forever $ atomically (readTChan rChan) >>= sendTextData

