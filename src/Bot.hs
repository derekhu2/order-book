{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Monad (when, void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands.Common
import State

runBot :: T.Text -> MVar State -> IO ()
runBot token stateMVar = do
    void $ forkIO $ forever $ do
        threadDelay (60 * 1000000) -- every 60 seconds
        statePath <- getStatePath
        saveState statePath stateMVar

    err <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler stateMVar
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
    TIO.putStrLn err

eventHandler :: MVar State -> Event -> DiscordHandler ()
eventHandler stateMVar event = case event of
    MessageCreate msg -> when (not (fromBot msg)) $ handlePrefixedCommand stateMVar msg
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor