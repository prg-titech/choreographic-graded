{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QualifiedDo               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Choreographic.Graded.Operation where


import           Choreographic.Graded.Choreography (CommInfo (..),
                                                    Communicatable, performIO,
                                                    receive, send)
import qualified Choreographic.Graded.Choreography as Choreography
import qualified Choreographic.Graded.Located      as Located
import           Choreographic.Graded.Location     (AllKnownSymbols, IsMember,
                                                    IsSubset, allKnownSymbols)
import qualified Control.Functor.Graded            as CFG
import           Control.Monad                     (forM_, when)
import           Data.Proxy                        (Proxy (..))
import           Data.Reflection                   (reifySymbol)
import           Data.Type.Set                     (IsSet, Nub, Sort, (:++))
import           GHC.TypeLits                      (KnownSymbol, symbolVal)
import           Prelude                           (Bool (..), Eq (..), IO,
                                                    error, otherwise, pure, ($))
import qualified Prelude                           as P

unsafeGetCurrentProcess :: Choreography.Choreography univ ps P.String
unsafeGetCurrentProcess = Choreography.Choreography \_ p -> pure p

-- | Communication from a process "p"
comm ::
  forall p ps univ a.
  (IsSet univ, IsSet ps, KnownSymbol p
    , Choreographic.Graded.Location.IsMember p univ ~ 'True, Choreographic.Graded.Location.AllKnownSymbols univ
    , Choreographic.Graded.Location.IsSubset ps univ ~ 'True
    ) =>
  (Communicatable a) =>
  Located.Located univ ps a ->
  Choreography.Choreography univ '[p] a
comm located =
  let
    pStr = symbolVal $ Proxy @p
  in
    Choreography.Choreography \univ current ->
      case located of
        Located.Located a
          | pStr == current -> do
            -- Send message to the process
            forM_ univ \p' -> do
              when (p' /= pStr) $ do
                let commInfo = CommInfo { ciFrom = pStr, ciTo = p' }
                send commInfo a
            pure a
          | otherwise -> do
            -- Receive message from the process
            let commInfo = CommInfo { ciFrom = pStr, ciTo = current }
            receive commInfo
        Located.Unlocated
          | pStr == current -> error "comm: Unlocated value cannot be sent"
          | otherwise -> do
            let commInfo = CommInfo { ciFrom = pStr, ciTo = current }
            receive commInfo

conclave ::
  forall univ ps a.
  (IsSet univ, IsSet ps, Choreographic.Graded.Location.IsSubset ps univ ~ 'True, Choreographic.Graded.Location.AllKnownSymbols ps) =>
  Located.Located univ ps (Choreography.Choreography univ ps a)->
  Choreography.Choreography univ ps (Located.Located univ ps a)
conclave located = Choreography.Choreography \univ p -> let ps = Choreographic.Graded.Location.allKnownSymbols @ps
  in case located of
  Located.Located (Choreography.Choreography mkProcess)
    | p `P.elem` ps -> do
        -- Run the choreography for the conclave
        a <- mkProcess ps p
        pure $ Located.Located a
    | otherwise -> pure Located.Unlocated
  Located.Unlocated
    -> pure Located.Unlocated

local :: forall p univ a b.
  (IsSet univ, KnownSymbol p) =>
  Located.Located univ '[p] a ->
  (a -> IO b)  ->
  Choreography.Choreography univ '[p] (Located.Located univ '[p] b)
local located ioAction = Choreography.Choreography \_ p -> case located of
  Located.Located a
    | p == symbolVal (Proxy @p) -> do
        -- Perform the IO action at the local process
        b <- performIO (ioAction a)
        pure $ Located.Located b
    | otherwise -> do
        -- Other processes return Unlocated (they don't have this value)
        pure Located.Unlocated
  Located.Unlocated -> pure Located.Unlocated
