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
import qualified Choreographic.Graded.Faced        as Faced
import qualified Choreographic.Graded.Located      as Located
import           Choreographic.Graded.Location     (AllKnownSymbols (..),
                                                    IsMember, IsSubset)
import qualified Control.Functor.Graded            as CFG
import           Control.Monad                     (forM, forM_, when)
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
    , IsMember p univ ~ 'True, AllKnownSymbols univ
    , IsMember p ps ~ 'True
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

enclave ::
  forall univ ps a.
  (IsSet univ, IsSet ps, IsSubset ps univ ~ 'True, AllKnownSymbols ps) =>
  Located.Located univ ps (Choreography.Choreography univ ps a)->
  Choreography.Choreography univ ps (Located.Located univ ps a)
enclave located = Choreography.Choreography \univ p -> case located of
  Located.Located (Choreography.Choreography mkProcess)
    | p `P.elem` allKnownSymbols (Proxy @ps) -> do
        -- Run the choreography for the enclave
        a <- mkProcess univ p
        pure $ Located.Located a
    | otherwise -> error "enclave: Current process is not a member of the enclave"
  Located.Unlocated
    -> pure Located.Unlocated

local :: forall univ ps a p b.
  (IsSet univ, IsSet ps, KnownSymbol p) =>
  Located.Located univ '[p] a ->
  (a -> IO b)  ->
  Choreography.Choreography univ ps (Located.Located univ '[p] b)
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


-- | Convert a Located value to a Faced value
faced ::
  forall univ ps a.
  Located.Located univ ps a ->
  Choreography.Choreography univ ps (Faced.Faced univ ps a)
faced located = Choreography.Choreography \_ _ -> case located of
  Located.Located a -> pure $ Faced.Faced a
  Located.Unlocated -> pure Faced.Unfaced

-- | Apply an IO operation to each location's value in a Faced value
foreach ::
  forall univ ps ps' a b.
  (AllKnownSymbols ps, IsSubset ps ps' ~ True, Nub
                      (Sort (ps' :++ ps'))
                    ~ ps') =>
  Faced.Faced univ ps a ->
  (forall p. KnownSymbol p => Located.Located univ '[p] a -> Choreography.Choreography univ ps' b) ->
  Choreography.Choreography univ ps' (Faced.Faced univ ps b)
foreach facedValue choreoFunc =
  let
    eachChoreo :: forall p'. KnownSymbol p' => Proxy p' -> Choreography.Choreography univ ps' b
    eachChoreo _ = choreoFunc @p' $ case facedValue of
      Faced.Faced a -> Located.Located a
      Faced.Unfaced -> Located.Unlocated

    stringToChoreo :: P.String -> Choreography.Choreography univ ps' b
    stringToChoreo pStr = reifySymbol pStr eachChoreo

    psSyms = allKnownSymbols (Proxy @ps)
  in CFG.do
    current <- (unsafeGetCurrentProcess :: Choreography.Choreography univ ps' P.String)
    justAndMaybe <- CFG.forM psSyms $ \pStr ->
      if pStr == current then
        P.Just CFG.<$> stringToChoreo pStr
      else CFG.do
        _ <- stringToChoreo pStr
        (CFG.sub $ CFG.return P.Nothing :: Choreography.Choreography univ ps' (P.Maybe b))
    case P.foldr (\x acc -> case x of P.Just v -> P.Just v; P.Nothing -> acc) P.Nothing justAndMaybe of
      P.Just v  -> (CFG.sub $ CFG.pure $ Faced.Faced v :: Choreography.Choreography univ ps' (Faced.Faced univ ps b))
      P.Nothing -> (CFG.sub $ CFG.pure Faced.Unfaced :: Choreography.Choreography univ ps' (Faced.Faced univ ps b))

-- | Collect all values from a Faced value into a Located list
collect ::
  forall univ ps a.
  (IsSet univ, IsSet ps, IsSubset ps univ ~ 'True, AllKnownSymbols ps, Communicatable a) =>
  Faced.Faced univ ps a ->
  Choreography.Choreography univ ps (Located.Located univ ps [a])
collect facedValue = Choreography.Choreography \_ p ->
  let psSyms = allKnownSymbols (Proxy @ps)
  in case facedValue of
    Faced.Faced a
      | p `P.elem` psSyms -> do
          -- Send this location's value to all other locations in ps
          forM_ psSyms \p' -> do
            when (p' /= p) $ do
              let commInfo = CommInfo { ciFrom = p, ciTo = p' }
              send commInfo a
          -- Receive values from all other locations in ps
          values <- P.mapM (\p' ->
            if p' == p
              then pure a
              else do
                let commInfo = CommInfo { ciFrom = p', ciTo = p }
                receive commInfo
            ) psSyms
          pure $ Located.Located values
      | otherwise -> do
          -- Receive values from all locations in ps
          values <- P.mapM (\p' -> do
            let commInfo = CommInfo { ciFrom = p', ciTo = p }
            receive commInfo
            ) psSyms
          pure $ Located.Located values
    Faced.Unfaced -> pure Located.Unlocated
