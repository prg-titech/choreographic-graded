{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Choreographic.Graded          (Choreography,
                                                CommunicationHooks (..), Faceted)
import qualified Choreographic.Graded          as C
import           Choreographic.Graded.Location ()
import           Control.Functor.Graded        (GradedComonadApply (new))
import qualified Control.Functor.Graded        as CFG
import qualified Data.Map                      as Map
import qualified Data.Type.Set                 as TS
import           Prelude                       (Bool (..), Eq (..), IO, Int,
                                                Maybe (..), Ord (..), Read (..),
                                                Show (..), String, elem,
                                                getLine, mapM_, maximum, print,
                                                putStrLn, read, return, show,
                                                take, words, ($), (++), (.))

-- Auction participants
type Univ = TS.AsSet '["auctioneer", "bidder1", "bidder2", "bidder3"]

-- Individual participant types
type Auctioneer = '["auctioneer"]
type Bidder1 = '["bidder1"]
type Bidder2 = '["bidder2"]
type Bidder3 = '["bidder3"]

-- All bidders
type Bidders = TS.AsSet '["bidder1", "bidder2", "bidder3"]

-- Auction data types
data AuctionItem = AuctionItem
  { itemName        :: String
  , itemDescription :: String
  , startingPrice   :: Int
  } deriving (Show, Read)

data Bid = Bid
  { bidder :: String
  , amount :: Int
  } deriving (Show, Read)

data AuctionResult = AuctionResult
  { winner     :: String
  , winningBid :: Int
  , allBids    :: [Bid]
  } deriving (Show, Read)

-- Get bid input from each bidder locally
getBidInput :: String -> IO Int
getBidInput itemName = do
  putStrLn $ "=== AUCTION: " ++ itemName ++ " ==="
  putStrLn "Enter your bid amount (0 to pass): "
  bidStr <- getLine
  return (read bidStr)

-- Evaluate all bids and determine winner
evaluateBids :: [Bid] -> AuctionResult
evaluateBids bids =
  let validBids = [bid | bid <- bids, amount bid > 0]
      winner = case validBids of
        [] -> AuctionResult "nobody" 0 bids
        _  -> let maxBid = maximum [amount bid | bid <- validBids]
                  winners = [bid | bid <- validBids, amount bid == maxBid]
              in case winners of
                   [winnerBid] -> AuctionResult (bidder winnerBid) (amount winnerBid) bids
                   _           -> AuctionResult "tie" maxBid bids
  in winner

-- Main auction choreography
auctionProgram :: Choreography Univ Univ AuctionResult
auctionProgram = CFG.sub $ CFG.do
  let
    -- Each bidder gets local input for their bid
    bidder1Input :: C.Located Univ Bidder1 AuctionItem
    bidder1Input = new $ AuctionItem "Vintage Guitar" "1959 Gibson Les Paul" 5000

    bidder2Input :: C.Located Univ Bidder2 AuctionItem
    bidder2Input = new $ AuctionItem "Vintage Guitar" "1959 Gibson Les Paul" 5000

    bidder3Input :: C.Located Univ Bidder3 AuctionItem
    bidder3Input = new $ AuctionItem "Vintage Guitar" "1959 Gibson Les Paul" 5000

  -- Phase 1: Get bid amounts locally from each bidder
  bidAmount1 <- C.local @Univ @Univ @AuctionItem @"bidder1" bidder1Input (getBidInput . itemName)
  bidAmount2 <- C.local @Univ @Univ @AuctionItem @"bidder2" bidder2Input (getBidInput . itemName)
  bidAmount3 <- C.local @Univ @Univ @AuctionItem @"bidder3" bidder3Input (getBidInput . itemName)

  -- Phase 2: Communicate raw bid amounts
  amount1 <- C.comm @"bidder1" bidAmount1
  amount2 <- C.comm @"bidder2" bidAmount2
  amount3 <- C.comm @"bidder3" bidAmount3

  -- Phase 3: Auctioneer evaluates all bids
  let
    bid1 = Bid "bidder1" amount1
    bid2 = Bid "bidder2" amount2
    bid3 = Bid "bidder3" amount3

    bids = [bid1, bid2, bid3]
    result :: C.Located Univ Auctioneer AuctionResult
    result = new $ evaluateBids bids

  -- Phase 4: Announce results
  C.comm @"auctioneer" result

main :: IO ()
main = do
  putStrLn "=== DISTRIBUTED AUCTION SYSTEM ==="
  putStrLn "Participants: auctioneer, bidder1, bidder2, bidder3"
  putStrLn ""

  let hooks = CommunicationHooks
        { onSend = \from to value ->
            putStrLn $ "[AUCTION] " ++ from ++ " -> " ++ to ++ " | " ++
                      (if "Bid" `elem` words value
                       then "Bid submitted"
                       else take 100 value)
        , onReceive = \from to value ->
            putStrLn $ "[RECV] " ++ from ++ " -> " ++ to ++ " | " ++
                      (if "AuctionResult" `elem` words value
                       then "Auction result received"
                       else "Data received")
        }

  finalResult <- C.runChoreographyConcurrentWithHooks hooks auctionProgram

  putStrLn ""
  putStrLn "=== AUCTION RESULTS ==="
  case Map.lookup "auctioneer" finalResult of
    (Just result) -> do
      putStrLn $ "Winner: " ++ winner result
      putStrLn $ "Winning Bid: $" ++ show (winningBid result)
      putStrLn "All Bids:"
      mapM_ (\bid -> putStrLn $ "  " ++ bidder bid ++ ": $" ++ show (amount bid)) (allBids result)
    Nothing -> putStrLn "Error: No auction result found"
