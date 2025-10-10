{-# LANGUAGE OverloadedStrings #-}

module Commands.Common (handlePrefixedCommand) where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Read (readMaybe)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands.Portfolio
import Commands.Market (makeMarket, buy, sell, showBook, cancel, resolve)
import State

data Command 
    = Ping
    | Bal
    | Setbal (Maybe Int) -- amount
    | Addbal (Maybe Int) -- amount
    | Transferbal (Maybe Int) -- amount
    | Assets
    | Makemarket T.Text T.Text -- assetName, resolution
    | Buy T.Text Bool Double Int  -- assetName, isYes, price, volume
    | Sell T.Text Bool Double Int  -- assetName, isYes, price, volume
    | ShowBook T.Text Bool  -- assetName, isYes
    | Cancel T.Text  -- assetName
    | Resolve T.Text Bool  -- assetName, isYes
    | Unknown T.Text
    deriving (Show, Eq)

parseCommand :: T.Text -> Command
parseCommand msg = 
    let tokens = T.words msg
    in case tokens of
        ("ping":_) -> Ping
        ("bal":_) -> Bal 
        ("setbal":amount:_) -> Setbal (readMaybe $ T.unpack amount)
        ("addbal":amount:_) -> Addbal (readMaybe $ T.unpack amount)
        ("transferbal":amount:_) -> Transferbal (readMaybe $ T.unpack amount)
        ("assets":_) -> Assets
        ("makemarket":assetName:rest) -> 
            let resolutionInfo = T.unwords rest
            in if T.null resolutionInfo
                then Unknown msg
                else Makemarket assetName resolutionInfo
        ("buy":assetName:yesNoText:priceText:volumeText:_) ->
            case (parseYesNo yesNoText, readMaybe $ T.unpack priceText, readMaybe $ T.unpack volumeText) of
                (Just isYes, Just price, Just volume) -> Buy assetName isYes price volume
                _ -> Unknown msg
        ("sell":assetName:yesNoText:priceText:volumeText:_) ->
            case (parseYesNo yesNoText, readMaybe $ T.unpack priceText, readMaybe $ T.unpack volumeText) of
                (Just isYes, Just price, Just volume) -> Sell assetName isYes price volume
                _ -> Unknown msg
        ("showbook":assetName:yesNoText:_) ->
            case parseYesNo yesNoText of
                Just isYes -> ShowBook assetName isYes
                _ -> Unknown msg
        ("cancel":assetName:_) -> Cancel assetName
        ("resolve":assetName:yesNoText:_) ->
            case parseYesNo yesNoText of
                Just isYes -> Resolve assetName isYes
                _ -> Unknown msg
        _ -> Unknown msg

parseYesNo :: T.Text -> Maybe Bool
parseYesNo text = case T.toLower text of
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

formatAssets :: M.Map T.Text Int -> T.Text
formatAssets assetsMap = 
    if M.null assetsMap
        then "no assets"
        else T.intercalate "\n" $ map formatAsset $ M.toList assetsMap
  where
    formatAsset (assetName, amount) = assetName <> ": " <> T.pack (show amount)

handleCommand :: MVar State -> Message -> DiscordHandler ()
handleCommand stateMVar msg = case parseCommand (messageContent msg) of
    Ping -> void $ restCall (R.CreateMessage (messageChannelId msg) "pong")

    Bal -> 
        case getFirstMention msg of
            Just uid -> do
                mbBalance <- liftIO $ getBalance stateMVar uid
                void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                    "balance: " <> T.pack (show mbBalance))
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs bal @user'")
   
    Setbal (Just amount) -> 
        case getFirstMention msg of
            Just uid -> do
                success <- liftIO $ setBalance stateMVar uid amount
                let response = if success
                    then "balance set to " <> T.pack (show amount)
                    else "failed to set balance (cannot be negative)"
                void $ restCall (R.CreateMessage (messageChannelId msg) response)
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs setbal <amount> @user'")
    
    Addbal (Just amount) -> 
        case getFirstMention msg of
            Just uid -> do
                success <- liftIO $ addBalance stateMVar uid amount
                let response = if success
                    then T.pack (show amount) <> " added to balance"
                    else "failed to add balance (cannot be negative)"
                void $ restCall (R.CreateMessage (messageChannelId msg) response)
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

    Assets ->
        case getFirstMention msg of
            Just uid -> do
                assetsMap <- liftIO $ getAssets stateMVar uid
                void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                    "assets: " <> formatAssets assetsMap)
            Nothing -> 
                void $ restCall (R.CreateMessage (messageChannelId msg) 
                    "use syntax: 'ob-hs assets @user'")
    
    Makemarket assetName resolutionInfo -> do
        liftIO $ makeMarket stateMVar assetName resolutionInfo
        void $ restCall (R.CreateMessage (messageChannelId msg) $ 
            "market created: " <> assetName <> " with resolution: " <> resolutionInfo)
    
    Buy assetName isYes price volume -> do
        let uid = userId $ messageAuthor msg
        success <- liftIO $ buy stateMVar assetName isYes price volume uid
        let response = if success 
                        then "bid placed: " <> assetName <> " " <> 
                             (if isYes then "YES" else "NO") <> " at " <> 
                             T.pack (show price) <> "$ for " <> T.pack (show volume)
                        else "bid failed"
        void $ restCall (R.CreateMessage (messageChannelId msg) response)
    
    Sell assetName isYes price volume -> do
        let uid = userId $ messageAuthor msg
        success <- liftIO $ sell stateMVar assetName isYes price volume uid
        let response = if success 
                        then "ask placed: " <> assetName <> " " <> 
                             (if isYes then "YES" else "NO") <> " at " <> 
                             T.pack (show price) <> "$ for " <> T.pack (show volume)
                        else "ask failed"
        void $ restCall (R.CreateMessage (messageChannelId msg) response)
    
    ShowBook assetName isYes -> do
        maybeBookDisplay <- liftIO $ showBook stateMVar assetName isYes
        case maybeBookDisplay of
            Just bookDisplay -> void $ restCall (R.CreateMessage (messageChannelId msg) bookDisplay)
            Nothing -> void $ restCall (R.CreateMessage (messageChannelId msg) $ 
                "Market not found: " <> assetName)
    
    Cancel assetName -> do
        let uid = userId $ messageAuthor msg
        success <- liftIO $ cancel stateMVar assetName uid
        let response = if success 
                        then "cancelled all orders for " <> assetName
                        else "failed to cancel orders"
        void $ restCall (R.CreateMessage (messageChannelId msg) response)
    
    Resolve assetName isYes -> do
        success <- liftIO $ resolve stateMVar assetName isYes
        let response = if success 
                        then assetName <> " resolved " <> 
                             (if isYes then "YES" else "NO")
                        else "failed to resolve market (market not found)"
        void $ restCall (R.CreateMessage (messageChannelId msg) response)
    
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