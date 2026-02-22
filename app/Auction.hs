{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE QualifiedDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Choreographic.Graded          (Choreography,
                                                CommunicationHooks (..))
import qualified Choreographic.Graded          as C
import           Choreographic.Graded.Location (AllKnownSymbols, IsMember,
                                                IsSubset)
import           Control.Functor.Graded        (GradedComonadApply (new))
import qualified Control.Functor.Graded        as CFG
import           Data.Data                     (Proxy (..))
import qualified Data.Map                      as Map
import           Data.Type.Set                 (IsSet)
import qualified Data.Type.Set                 as TS
import           GHC.TypeLits                  (KnownSymbol, symbolVal)


-- Auction participants
type Univ = TS.AsSet '["auctioneer", "bidder1", "bidder2", "bidder3"]

-- Individual participant types
type Auctioneer = '["auctioneer"]
type Bidder1 = '["bidder1"]
type Bidder2 = '["bidder2"]
type Bidder3 = '["bidder3"]

-- All bidders
type Bidders = TS.AsSet '["bidder1", "bidder2", "bidder3"]
data Bid = Bid
  { bidder :: String
  , amount :: Int
  } deriving (Show, Read)

data AuctionResult = AuctionResult
  { winner     :: String
  , winningBid :: Int
  , allBids    :: [Bid]
  } deriving (Show, Read)

getBidInput :: String -> IO Int
getBidInput item = do
  putStrLn $ "=== AUCTION: " ++ item ++ " ==="
  putStrLn "Enter your bid amount (0 to pass): "
  read <$> getLine

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

getAndSendBid
  :: forall p
  . (KnownSymbol p
  , IsMember p Univ ~ 'True
  , IsSubset '[p] Univ ~ 'True
  , IsSubset '[p] (TS.AsSet ["auctioneer", p]) ~ 'True
  , TS.IsSet (TS.AsSet ["auctioneer", p])
  , IsSubset (TS.AsSet ["auctioneer", p]) Univ ~ 'True
  , AllKnownSymbols (TS.AsSet ["auctioneer", p])
  )
  => C.Located Univ '[p] String
  -> Choreography Univ
    (TS.AsSet ["auctioneer", p])
    (C.Located Univ (TS.AsSet ["auctioneer", p]) Bid)
getAndSendBid name =
  let
    comp
      :: C.Located Univ (TS.AsSet ["auctioneer", p])
        (Choreography Univ (TS.AsSet ["auctioneer", p]) Bid)
    comp = new $ CFG.sub $ CFG.do
        bidAmount1 <- C.local @p name getBidInput
        C.comm @p $ Bid (symbolVal (Proxy @p)) CFG.<$> bidAmount1
  in C.conclave @Univ @(TS.AsSet ["auctioneer", p]) comp


-- Main auction choreography
auctionProgram :: Choreography Univ Univ AuctionResult
auctionProgram = CFG.sub $ CFG.do
  let
    auctionItem :: C.Located Univ Univ String
    auctionItem = new "Vintage Guitar"
  -- Phase 1: Get bid amounts locally from each bidder
  amount1 <- getAndSendBid @"bidder1" (CFG.sub auctionItem)
  amount2 <- getAndSendBid @"bidder2" (CFG.sub auctionItem)
  amount3 <- getAndSendBid @"bidder3" (CFG.sub auctionItem)

  -- Phase 3: Auctioneer evaluates all bids
  let
    bids = (\b1 b2 b3 -> [b1, b2, b3]) CFG.<$> CFG.sub amount1 CFG.<*> CFG.sub amount2 CFG.<*> CFG.sub amount3
    result :: C.Located Univ Auctioneer AuctionResult
    result = evaluateBids CFG.<$> bids

  -- Phase 4: Announce results
  C.comm @"auctioneer" result

main :: IO ()
main = do
  putStrLn "=== DISTRIBUTED AUCTION SYSTEM ==="
  putStrLn "Participants: auctioneer, bidder1, bidder2, bidder3"
  putStrLn ""

  let hooks = CommunicationHooks
        { onSend = \from to value ->
            putStrLn $ "[SEND] " ++ from ++ " -> " ++ to ++ " | " ++
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
