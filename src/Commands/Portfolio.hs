{-# LANGUAGE OverloadedStrings #-}

module Commands.Portfolio
    ( getBalance
    , setBalance
    , addBalance
    , transferBalance
    ) where

import State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_, modifyMVar)
import Data.Functor ((<&>))

import Discord.Types (UserId)

getBalance :: MVar State -> UserId -> IO (Maybe Int)
getBalance stateMVar userId = do
    state <- readMVar stateMVar
    return $ balance <$> M.lookup userId (users state)

setBalance :: MVar State -> UserId -> Int -> IO ()
setBalance stateMVar userId newBalance = 
    modifyMVar_ stateMVar $ \state -> do
        let newProfile = UserProfile { balance = newBalance }
        let newUsers = M.insert userId newProfile (users state)
        return state { users = newUsers }

addBalance :: MVar State -> UserId -> Int -> IO ()
addBalance stateMVar userId amount = 
    modifyMVar_ stateMVar $ \state -> do
        let current = M.lookup userId (users state) <&> balance
        let newBalance = maybe amount (+ amount) current
        let newProfile = UserProfile { balance = newBalance }
        let newUsers = M.insert userId newProfile (users state)
        return state { users = newUsers }

transferBalance :: MVar State -> UserId -> UserId -> Int -> IO Bool
transferBalance stateVar fromId toId amount = 
    modifyMVar stateVar $ \state ->
        let usersMap = users state
            fromBal = maybe 0 balance (M.lookup fromId usersMap)
            toBal = maybe 0 balance (M.lookup toId usersMap)
        in if fromBal >= amount && amount > 0
            then
                let updatedUsers = M.insert fromId (UserProfile (fromBal - amount))
                         $ M.insert toId (UserProfile (toBal + amount))
                         $ usersMap
                    newState = state { users = updatedUsers }
                in return (newState, True)
            else
                return (state, False)