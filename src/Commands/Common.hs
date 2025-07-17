{-# LANGUAGE OverloadedStrings #-}

module Commands.Common where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.List (find)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands.Portfolio
import State

data Command 
    = Ping
    | Bal
    | Setbal (Maybe Int)
    | Addbal (Maybe Int)
    | Transferbal (Maybe Int)
    | Unknown T.Text
    deriving (Show, Eq)

parseCommand :: T.Text -> Command
parseCommand msg = 
    let words = T.words msg
    in case words of
        ("ping":_) -> Ping
        ("bal":_) -> Bal 
        ("setbal":amount:_) -> Setbal (readMaybe $ T.unpack amount)
        ("addbal":amount:_) -> Addbal (readMaybe $ T.unpack amount)
        ("transferbal":amount:_) -> Transferbal (readMaybe $ T.unpack amount)
        _ -> Unknown msg

handleCommand :: MVar State -> Message -> DiscordHandler ()
handleCommand stateMVar msg = case parseCommand (messageContent msg) of
    Ping -> void $ restCall (R.CreateMessage (messageChannelId msg) "pong")

    Bal -> 
        case getFirstMention msg of
            Just userId -> do
                balance <- liftIO $ getBalance stateMVar userId
                void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                    "balance: " <> T.pack (show balance))
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs bal @user'")
    
    Setbal (Just amount) -> 
        case getFirstMention msg of
            Just userId -> do
                liftIO $ setBalance stateMVar userId amount
                void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                    "balance set to " <> T.pack (show amount))
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs setbal <amount> @user'")
    
    Addbal (Just amount) -> 
        case getFirstMention msg of
            Just userId -> do
                liftIO $ addBalance stateMVar userId amount
                void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                    T.pack (show amount) <> " added to balance")
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs addbal <amount> @user'")
    
    Transferbal (Just amount) -> 
        case getFirstTwoMentions msg of
            (Just fromId, Just toId) -> do
                success <- liftIO $ transferBalance stateMVar fromId toId amount
                let response = if success then "transfer good" else "transfer fail"
                void $ restCall (R.CreateMessage (messageChannelId msg) response)
            _ -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs transferbal <amount> @fromUser @toUser'")
    
    _ -> void $ restCall (R.CreateMessage (messageChannelId msg) "invalid command or arguments")

getFirstMention :: Message -> Maybe UserId
getFirstMention msg = 
    case messageMentions msg of
        (u:_) -> Just $ userId u
        _ -> Nothing

getFirstTwoMentions :: Message -> (Maybe UserId, Maybe UserId)
getFirstTwoMentions msg = 
    case messageMentions msg of
        (u1:u2:_) -> (Just $ userId u1, Just $ userId u2)
        (u:_) -> (Just $ userId u, Nothing)
        _ -> (Nothing, Nothing)

handlePrefixedCommand :: MVar State -> Message -> DiscordHandler ()
handlePrefixedCommand stateMVar msg = 
    let prefix = "ob-hs "
        content = T.stripStart (messageContent msg)
    in when (prefix `T.isPrefixOf` content) $ do
        let strippedContent = T.strip $ T.drop (T.length prefix) content
        let modifiedMsg = msg { messageContent = strippedContent }
        handleCommand stateMVar modifiedMsg