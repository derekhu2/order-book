{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getEnv)
import Bot (runBot)
import Utils (getToken, getGuildId)

main :: IO ()
main = do
    token <- getToken
    guildId <- getGuildId
    TIO.putStrLn $ "Starting bot... "
    runBot token