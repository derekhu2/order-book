{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Book 
    ( BidPriority(..)
    , AskPriority(..)
    , Order(..)
    , OrderBook(..)
    , Market(..)
    , MatchResult(..)
    
    -- empty
    , emptyOrderBook
    , emptyMarket

    -- place orders
    , addBid
    , addAsk
    , placeRemainingOrder

    -- match orders
    , matchAgainstAsks
    , matchAgainstBids

    -- display book formatting
    , formatOrderGroup
    , formatOrderLine
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..)
                       , (.:)
                       , (.=)
                       , object
                       , withObject
                       )
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.PQueue.Prio.Min as MinPQ
import qualified Data.PQueue.Prio.Max as MaxPQ
import Text.Printf (printf)

import Discord.Types (UserId)

-- bids: highest price, earliest time
data BidPriority = BidPriority
    { bidPrice :: Double
    , bidTime :: UTCTime
    } deriving (Show, Generic, Eq)

instance FromJSON BidPriority
instance ToJSON BidPriority

instance Ord BidPriority where
    compare (BidPriority p1 t1) (BidPriority p2 t2) =
        case compare p1 p2 of
            EQ -> compare t2 t1
            other -> other

-- asks: lowest price, earliest time
data AskPriority = AskPriority
    { askPrice :: Double
    , askTime :: UTCTime
    } deriving (Show, Generic, Eq)

instance FromJSON AskPriority
instance ToJSON AskPriority

instance Ord AskPriority where
    compare (AskPriority p1 t1) (AskPriority p2 t2) =
        case compare p1 p2 of
            EQ -> compare t1 t2
            other -> other

data OrderBook = OrderBook
    { bids :: MaxPQ.MaxPQueue BidPriority Order
    , asks :: MinPQ.MinPQueue AskPriority Order
    } deriving (Show, Generic)

data Order = Order
    { orderPrice :: Double
    , orderTime :: UTCTime
    , orderVolume :: Int
    , orderUserId :: UserId
    } deriving (Show, Generic)

instance FromJSON Order
instance ToJSON Order

instance ToJSON OrderBook where
    toJSON (OrderBook bidQ askQ) =
        let bidsList = MaxPQ.toList bidQ
            asksList = MinPQ.toList askQ
        in object [ "bids" .= bidsList
                   , "asks" .= asksList
                   ]

instance FromJSON OrderBook where
    parseJSON = withObject "OrderBook" $ \o -> do
        bidsList <- o .: "bids"
        asksList <- o .: "asks"
        let bidQ = MaxPQ.fromList bidsList
            askQ = MinPQ.fromList asksList
        return (OrderBook bidQ askQ)

data Market = Market
    { assetName :: T.Text
    , resolution :: T.Text
    , orderBook :: OrderBook
    } deriving (Show, Generic)

instance FromJSON Market
instance ToJSON Market

data MatchResult = MatchResult
    { filledVolume :: Int
    , remainingQueue :: OrderBook -> OrderBook
    , matches :: [(Order, Int)]
    }

emptyOrderBook :: OrderBook
emptyOrderBook = OrderBook MaxPQ.empty MinPQ.empty

emptyMarket :: T.Text -> T.Text -> Market
emptyMarket name res = Market name res emptyOrderBook

addBid :: Order -> OrderBook -> OrderBook
addBid order book = 
    let priority = BidPriority (orderPrice order) (orderTime order)
    in book { bids = MaxPQ.insert priority order (bids book) }

addAsk :: Order -> OrderBook -> OrderBook
addAsk order book = 
    let priority = AskPriority (orderPrice order) (orderTime order)
    in book { asks = MinPQ.insert priority order (asks book) }

viewAsk :: MinPQ.MinPQueue AskPriority Order -> Maybe ((AskPriority, Order), MinPQ.MinPQueue AskPriority Order)
viewAsk = MinPQ.minViewWithKey

viewBid :: MaxPQ.MaxPQueue BidPriority Order -> Maybe ((BidPriority, Order), MaxPQ.MaxPQueue BidPriority Order)
viewBid = MaxPQ.maxViewWithKey

insertAsk :: AskPriority -> Order -> MinPQ.MinPQueue AskPriority Order -> MinPQ.MinPQueue AskPriority Order
insertAsk = MinPQ.insert

insertBid :: BidPriority -> Order -> MaxPQ.MaxPQueue BidPriority Order -> MaxPQ.MaxPQueue BidPriority Order
insertBid = MaxPQ.insert

placeRemainingOrder :: OrderBook -> Double -> Int -> UserId -> Bool -> IO OrderBook
placeRemainingOrder book price remainingVolume userId placeBid = do
    if remainingVolume <= 0 
        then return book
        else do
            currentTime <- getCurrentTime
            let newOrder = Order price currentTime remainingVolume userId
            return $ if placeBid 
                then addBid newOrder book
                else addAsk newOrder book

matchOrders ::
               (queue -> Maybe ((priority, Order), queue)) 
             -> (Double -> UTCTime -> priority)
             -> (priority -> Order -> queue -> queue) 
             -> (Double -> Bool)
             -> Int
             -> queue
             -> (Int, queue, [(Order, Int)])
matchOrders viewFn makePriority insertFn priceCheck volume queue = matchOrders' volume queue []
  where
    matchOrders' 0 q acc = (volume, q, acc)
    matchOrders' remainingVolume q acc =
        case viewFn q of
            Nothing -> (volume - remainingVolume, q, acc)
            Just ((_, order), restQueue) ->
                if priceCheck (orderPrice order)
                    then (volume - remainingVolume, q, acc)
                    else 
                        let orderVol = orderVolume order
                            fillVolume = min remainingVolume orderVol
                            newRemainingVolume = remainingVolume - fillVolume
                        in if orderVol == fillVolume
                            then -- fully fill this order
                                matchOrders' newRemainingVolume restQueue ((order, fillVolume) : acc)
                            else -- partially fill this order
                                let partialOrder = order { orderVolume = orderVol - fillVolume }
                                    partialPriority = makePriority (orderPrice order) (orderTime order)
                                    newQueue = insertFn partialPriority partialOrder restQueue
                                in (volume, newQueue, (order, fillVolume) : acc)

-- match asks: want to buy YES, sell NO
matchAgainstAsks :: Double -> Int -> OrderBook -> MatchResult
matchAgainstAsks maxPrice volume book = 
    let (filled, newAsks, matchList) = matchOrders viewAsk AskPriority insertAsk (> maxPrice) volume (asks book)
    in MatchResult filled (\b -> b { asks = newAsks }) matchList

-- match bids: want to sell YES, buy NO
matchAgainstBids :: Double -> Int -> OrderBook -> MatchResult
matchAgainstBids minPrice volume book = 
    let (filled, newBids, matchList) = matchOrders viewBid BidPriority insertBid (< minPrice) volume (bids book)
    in MatchResult filled (\b -> b { bids = newBids }) matchList

-- format a group of orders for Discord text display
formatOrderGroup :: T.Text -> [(Double, Int)] -> T.Text
formatOrderGroup groupName orders = 
    let header = "**" <> groupName <> "**\n"
        columnHeader = "  Price   Volume\n"
    in if null orders
        then header <> "  (empty)\n"
        else header <> columnHeader <> T.concat [formatOrderLine price volume | (price, volume) <- orders]

-- format a single order line
formatOrderLine :: Double -> Int -> T.Text
formatOrderLine price volume = 
    T.pack $ printf "  $%.2f   %d\n" price volume