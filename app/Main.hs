{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getEnv)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Concurrent.MVar (MVar, newMVar)

import Bot (runBot)
import Utils (getToken, getGuildId)
import State

main :: IO ()
main = do
    statePath <- getStatePath
    stateMVar <- if doesFileExist statePath
        then loadState statePath
        else do
            TIO.putStrLn "creating new state file"
            newState <- newMVar emptyState
            saveState statePath newState
            return newState
            
    token <- getToken
    guildId <- getGuildId
    TIO.putStrLn $ "Starting bot... "
    runBot token stateMVar