{-# LANGUAGE OverloadedStrings #-}

module Commands.Market
    ( makeMarket
    , buy
    , sell
    , cancel
    , resolve
    , showBook
    ) where

import Control.Concurrent.MVar (MVar, readMVar)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as MinPQ
import qualified Data.PQueue.Prio.Max as MaxPQ
import qualified Data.List as L
import Data.Function (on)

import Discord.Types (UserId)
import State
import Book
import Commands.Portfolio (resolveAssets, addAsset, addBalance)

makeMarket :: MVar State -> T.Text -> T.Text -> IO ()
makeMarket stateMVar assetKey resolutionText = do
    setState stateMVar $ \state -> 
        state { markets = M.insert assetKey (emptyMarket assetKey resolutionText) (markets state) }

-- buying YES = selling NO; buying NO = selling YES
buy :: MVar State -> T.Text -> Bool -> Double -> Int -> UserId -> IO Bool
buy stateMVar assetKey isYes price volume userId = do
    let adjPrice = if isYes then price else 1 - price  -- NO price flips
    state <- readMVar stateMVar
    case M.lookup assetKey (markets state) of
        Nothing -> return False
        Just market -> do
            (success, updatedMarket) <- placeOrderInMarket stateMVar market isYes adjPrice volume userId
            if success then do
                setState stateMVar $ \s -> s { markets = M.insert assetKey updatedMarket (markets s) }
                return True
            else
                return False

sell :: MVar State -> T.Text -> Bool -> Double -> Int -> UserId -> IO Bool
sell stateMVar assetKey isYes price volume userId = do
    let adjPrice = if isYes then price else 1 - price
    state <- readMVar stateMVar
    case M.lookup assetKey (markets state) of
        Nothing -> return False
        Just market -> do
            (success, updatedMarket) <- placeOrderInMarket stateMVar market (not isYes) adjPrice volume userId
            if success then do
                setState stateMVar $ \s -> s { markets = M.insert assetKey updatedMarket (markets s) }
                return True
            else
                return False

-- buy/sell helpers
executeTrade :: MVar State -> T.Text -> Bool -> Double -> Int -> UserId -> UserId -> IO Bool
executeTrade stateMVar assetKey isYes price volume buyerId sellerId = do
    let tradeValue = round (price * fromIntegral volume)
    let assetAmount = if isYes then volume else (-volume)
    
    buyerBalanceSuccess <- addBalance stateMVar buyerId (-tradeValue)
    if not buyerBalanceSuccess
        then return False  
        else do
            -- buyer
            addAsset stateMVar buyerId assetKey assetAmount
            
            -- seller
            _ <- addBalance stateMVar sellerId tradeValue
            addAsset stateMVar sellerId assetKey (-assetAmount)
            return True

executeMatches :: MVar State -> T.Text -> Bool -> [(Order, Int)] -> UserId -> IO (Bool, Int)
executeMatches stateMVar assetKey isYes matchedPairs userId = do
    results <- mapM handleMatch matchedPairs
    let successes   = map fst results
        volumes     = map snd results
        totalSuccess = and successes
        totalVolume  = if totalSuccess then sum volumes else 0
    return (totalSuccess, totalVolume)
  where
    handleMatch (matchedOrder, fillVol) = do
        success <- executeTrade stateMVar assetKey isYes (orderPrice matchedOrder) fillVol userId (orderUserId matchedOrder)
        pure (success, fillVol)

-- place order in market (returns updated market, doesn't modify state)
-- isBuyingYes = True for (buy YES, sell NO), False for (buy NO, sell YES)
placeOrderInMarket :: MVar State -> Market -> Bool -> Double -> Int -> UserId -> IO (Bool, Market)
placeOrderInMarket stateMVar market isBuyingYes price volume userId = do
    let book = orderBook market
    
    let matchResult = if isBuyingYes 
            then matchAgainstAsks price volume book  -- buy YES: match against asks
            else matchAgainstBids price volume book  -- buy NO: match against bids
    
    -- execute all matched trades
    (allTradesSuccess, actualFilledVolume) <- executeMatches stateMVar (assetName market) True (matches matchResult) userId
    
    if not allTradesSuccess then
        return (False, market)
    else do
        -- update the order book with remaining orders after matches
        let updatedBook = (remainingQueue matchResult) book
        
        -- place remaining volume
        let remainingVol = volume - actualFilledVolume
        finalBook <- placeRemainingOrder updatedBook price remainingVol userId isBuyingYes
        
        let updatedMarket = market { orderBook = finalBook }
        return (True, updatedMarket)

cancel :: MVar State -> T.Text -> UserId -> IO Bool
cancel stateMVar assetKey userId = do
    setState stateMVar $ \state ->
        case M.lookup assetKey (markets state) of
            Nothing -> state
            Just market -> 
                let book = orderBook market
                    filteredBids = MaxPQ.filter (\order -> orderUserId order /= userId) (bids book)
                    filteredAsks = MinPQ.filter (\order -> orderUserId order /= userId) (asks book)
                    updatedBook = book { bids = filteredBids, asks = filteredAsks }
                    updatedMarket = market { orderBook = updatedBook }
                in state { markets = M.insert assetKey updatedMarket (markets state) }
    
    return True

resolve :: MVar State -> T.Text -> Bool -> IO Bool
resolve stateMVar assetKey isYes = do
    state <- readMVar stateMVar
    case M.lookup assetKey (markets state) of
        Nothing -> return False
        Just _ -> do
            let usersWithAssets = M.keys $ M.filter (M.member assetKey . assets) (users state)
            mapM_ (\userId -> resolveAssets stateMVar userId assetKey isYes) usersWithAssets
            setState stateMVar $ \s -> s { markets = M.delete assetKey (markets s) }
            return True

showBook :: MVar State -> T.Text -> Bool -> IO (Maybe T.Text)
showBook stateMVar assetKey isYesView = do
    state <- readMVar stateMVar
    case M.lookup assetKey (markets state) of
        Nothing -> return Nothing
        Just market -> do
            let book = orderBook market
            let marketAssetName = assetName market
            let res = resolution market

            let rawAsks = MinPQ.toList (asks book)
                rawBids = MaxPQ.toList (bids book)

            -- helper to summarise prices
            let summarise flipP xs =
                    let adjust o = let p = orderPrice o in if flipP then 1 - p else p
                    in M.toList $ M.fromListWith (+) [ (adjust o, orderVolume o) | (_,o) <- xs ]

                sortDesc = L.sortBy (flip compare `on` fst)

            -- flip view for NO
            let (asksSummary, bidsSummary) = if isYesView
                    then ( summarise False rawAsks
                         , summarise False rawBids )
                    else ( summarise True  rawBids  -- bids become asks, vice versa
                         , summarise True  rawAsks )

                groupedAsks = sortDesc asksSummary
                groupedBids = sortDesc bidsSummary

            -- build display
            let header = "**" <> marketAssetName <> "**" <> ": " <> res <> "\n"
                columnHeader = "  Price   Volume\n"
                asksLines = T.concat [formatOrderLine p v | (p,v) <- groupedAsks]
                bidsLines = T.concat [formatOrderLine p v | (p,v) <- groupedBids]
                separator = "---------------------------------\n"
                display = header <> columnHeader <> asksLines <> separator <> bidsLines

            return $ Just display