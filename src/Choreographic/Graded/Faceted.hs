{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Choreographic.Graded.Faceted where

import           Choreographic.Graded.Choreography (Choreography)
import           Choreographic.Graded.Located      (Located)
import           Choreographic.Graded.Location     (All (unconsWithAll),
                                                    AllKnownSymbols)
import           Control.Functor.Graded            (SubGradable (sub))
import qualified Control.Functor.Graded            as CFG
import           Data.Proxy                        (Proxy (..))
import qualified Data.Type.Set                     as TS
import           GHC.TypeLits

-- | Faceted values can differ between locations
-- Unlike Located values which are uniform, Faceted values
-- represent location-specific data that may be different at each location
data FacetedImpl (univ :: [Symbol]) a (ps :: [Symbol]) where
  FacetedNil :: FacetedImpl univ a '[]
  FacetedCons ::
    (KnownSymbol p) =>
    Located univ '[p] a
    -> FacetedImpl univ a ps'
    -> FacetedImpl univ a (p : ps')

newtype Faceted univ ps a = Faceted (FacetedImpl univ a ps)

faceted :: forall univ ps a.
   AllKnownSymbols ps => Located univ ps a -> Faceted univ ps a
faceted located = Faceted $ go located
 where
  go :: forall univ' ps' a'. AllKnownSymbols ps' => Located univ' ps' a' -> FacetedImpl univ' a' ps'
  go loc = unconsWithAll
    (Proxy @(FacetedImpl univ' a'))
    (Proxy @KnownSymbol)
    (Proxy @ps')
    (\_ _ -> FacetedCons (sub loc) (go (sub loc)))
    undefined

-- | Apply an operation to each location's value in a Faceted value
foreach ::
  forall univ ps ps' a b.
  (TS.Union ps' ps' ~ ps') =>
  Faceted univ ps a ->
  (forall p. KnownSymbol p => Located univ '[p] a -> Choreography univ ps' (Located univ '[p] b)) ->
  Choreography univ ps' (Faceted univ ps b)
foreach (Faceted facetedImpl) choreoFunc = case facetedImpl of
  FacetedNil                    -> sub $ CFG.pure $ Faceted $ FacetedNil
  FacetedCons located facetedTail -> CFG.do
    newLocated <- choreoFunc located
    newFacetedTail <- foreach (Faceted facetedTail) choreoFunc
    let Faceted newFacetedImplTail = newFacetedTail
    sub $ CFG.pure $ Faceted $ FacetedCons newLocated newFacetedImplTail
      :: Choreography univ ps' (Faceted univ ps b)
