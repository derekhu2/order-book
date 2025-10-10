{-# LANGUAGE DeriveGeneric #-}

module State
    ( UserProfile(..)
    , State(..)
    , emptyState
    , loadState
    , saveState
    , setState
    , getStatePath
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(..))
import qualified Data.Text as T

import Discord.Types (UserId)
import Book (Market)

data UserProfile = UserProfile
    { balance :: Int
    , assets :: Map T.Text Int
    } deriving (Show, Generic)

instance FromJSON UserProfile
instance ToJSON UserProfile

data State = State 
    { users :: Map UserId UserProfile
    , markets :: Map T.Text Market
    } deriving (Show, Generic)

instance FromJSON State
instance ToJSON State

emptyState :: State
emptyState = State M.empty M.empty

-- | helpers

loadState :: FilePath -> IO (MVar State)
loadState path = do
    content <- BSL.readFile path
    let initialState = case decode content of
            Just s -> s
            Nothing -> emptyState
    newMVar initialState

saveState :: FilePath -> MVar State -> IO ()
saveState path stateMVar = do
    state <- readMVar stateMVar
    BSL.writeFile path $ encode state

setState :: MVar State -> (State -> State) -> IO ()
setState stateMVar f = modifyMVar_ stateMVar (pure . f)

getStatePath :: IO FilePath
getStatePath = do
    dir <- getXdgDirectory XdgData "order-book"
    createDirectoryIfMissing True dir
    return (dir <> "/state.json")