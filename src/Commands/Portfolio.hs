{-# LANGUAGE OverloadedStrings #-}

module Commands.Portfolio
    ( getBalance
    , setBalance
    , addBalance
    , transferBalance
    , getAssets
    , setAssets
    , addAsset
    , resolveAssets
    ) where

import State
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_, modifyMVar)

import Discord.Types (UserId)

getBalance :: MVar State -> UserId -> IO (Maybe Int)
getBalance stateMVar userId = do
    state <- readMVar stateMVar
    return $ balance <$> M.lookup userId (users state)

setBalance :: MVar State -> UserId -> Int -> IO Bool
setBalance stateMVar userId newBalance = 
    if newBalance < 0
        then return False
        else do
            modifyMVar_ stateMVar $ \state -> do
                let existingProfile = M.lookup userId (users state)
                let existingAssets = maybe M.empty assets existingProfile
                let newProfile = UserProfile { balance = newBalance, assets = existingAssets }
                let newUsers = M.insert userId newProfile (users state)
                return state { users = newUsers }
            return True

addBalance :: MVar State -> UserId -> Int -> IO Bool
addBalance stateMVar userId amount = 
    modifyMVar stateMVar $ \state -> do
        let existingProfile = M.lookup userId (users state)
        let currentBalance = maybe 0 balance existingProfile
        let newBalance = currentBalance + amount
        
        if newBalance < 0
            then return (state, False)
            else do
                let existingAssets = maybe M.empty assets existingProfile
                let newProfile = UserProfile { balance = newBalance, assets = existingAssets }
                let newUsers = M.insert userId newProfile (users state)
                return (state { users = newUsers }, True)

transferBalance :: MVar State -> UserId -> UserId -> Int -> IO Bool
transferBalance stateVar fromId toId amount = 
    modifyMVar stateVar $ \state ->
        let usersMap = users state
            fromProfile = M.lookup fromId usersMap
            toProfile = M.lookup toId usersMap
            fromBal = maybe 0 balance fromProfile
            toBal = maybe 0 balance toProfile
            fromAssets = maybe M.empty assets fromProfile
            toAssets = maybe M.empty assets toProfile
        in if fromBal >= amount && amount > 0
            then
                let updatedUsers = M.insert fromId (UserProfile (fromBal - amount) fromAssets)
                         $ M.insert toId (UserProfile (toBal + amount) toAssets)
                         $ usersMap
                    newState = state { users = updatedUsers }
                in return (newState, True)
            else
                return (state, False)

getAssets :: MVar State -> UserId -> IO (Map T.Text Int)
getAssets stateMVar userId = do
    state <- readMVar stateMVar
    return $ maybe M.empty assets (M.lookup userId (users state))

setAssets :: MVar State -> UserId -> Map T.Text Int -> IO ()
setAssets stateMVar userId newAssets = do
    modifyMVar_ stateMVar $ \state -> do
        let existingProfile = M.lookup userId (users state)
        let currentBalance = maybe 0 balance existingProfile
        let newProfile = UserProfile { balance = currentBalance, assets = newAssets }
        let newUsers = M.insert userId newProfile (users state)
        return state { users = newUsers }

addAsset :: MVar State -> UserId -> T.Text -> Int -> IO ()
addAsset stateMVar userId assetName amount = do
    modifyMVar_ stateMVar $ \state -> do
        let existingProfile = M.lookup userId (users state)
        let currentBalance = maybe 0 balance existingProfile
        let currentAssets = maybe M.empty assets existingProfile
        let newAssets = M.insertWith (+) assetName amount currentAssets
        let newProfile = UserProfile { balance = currentBalance, assets = newAssets }
        let newUsers = M.insert userId newProfile (users state)
        return state { users = newUsers }

-- resolve markets into balance/assets(YES -> positives worth 1, NO -> negatives worth 1)
resolveAssets :: MVar State -> UserId -> T.Text -> Bool -> IO ()
resolveAssets stateMVar userId assetName isYes = do
    modifyMVar_ stateMVar $ \state -> do
        let existingProfile = M.lookup userId (users state)
        let currentBalance = maybe 0 balance existingProfile
        let currentAssets = maybe M.empty assets existingProfile
        
        case M.lookup assetName currentAssets of
            Nothing -> return state
            Just assetAmount -> do
                let payout = if isYes
                    then if assetAmount > 0 then assetAmount else 0
                    else if assetAmount < 0 then (-assetAmount) else 0
                
                let newBalance = currentBalance + payout
                let newAssets = M.delete assetName currentAssets
                let newProfile = UserProfile { balance = newBalance, assets = newAssets }
                let newUsers = M.insert userId newProfile (users state)
                return state { users = newUsers }