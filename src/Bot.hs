{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Monad (when, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands

runBot :: T.Text -> IO ()
runBot token = do
    err <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
    TIO.putStrLn err

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor