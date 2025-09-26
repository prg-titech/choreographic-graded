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

module Choreographic.Graded.Faced where

import           Choreographic.Graded.Choreography (Choreography,
                                                    Communicatable, send)
import           Choreographic.Graded.Located      (Located)
import           Choreographic.Graded.Location     (All (unconsWithAll),
                                                    AllKnownSymbols, IsMember,
                                                    IsSubset)
import           Choreographic.Graded.Operation    (comm)
import           Control.Functor.Graded            (GradedFunctor (..),
                                                    GradedMonad (..),
                                                    SubGradable (sub))
import qualified Control.Functor.Graded            as CFG
import           Data.Proxy                        (Proxy (..))
import qualified Data.Type.Set                     as TS
import           GHC.Exts                          (Symbol)
import           GHC.TypeLits

-- | Faced values can differ between locations
-- Unlike Located values which are uniform, Faced values
-- represent location-specific data that may be different at each location
data FacedImpl (univ :: [Symbol]) a (ps :: [Symbol]) where
  FacedNil :: FacedImpl univ a '[]
  FacedCons ::
    (KnownSymbol p) =>
    Located univ '[p] a
    -> FacedImpl univ a ps'
    -> FacedImpl univ a (p : ps')

newtype Faced univ ps a = Faced (FacedImpl univ a ps)

faced :: forall univ ps a.
   AllKnownSymbols ps => Located univ ps a -> Faced univ ps a
faced located = Faced $ go located
 where
  go :: forall univ ps a. AllKnownSymbols ps => Located univ ps a -> FacedImpl univ a ps
  go loc = unconsWithAll
    (Proxy @(FacedImpl univ a))
    (Proxy @KnownSymbol)
    (Proxy @ps)
    (\_ _ -> FacedCons (sub loc) (go (sub loc)))
    undefined

-- | Apply an operation to each location's value in a Faced value
foreach ::
  forall univ ps ps' a b.
  (TS.Union ps' ps' ~ ps') =>
  Faced univ ps a ->
  (forall p. KnownSymbol p => Located univ '[p] a -> Choreography univ ps' (Located univ '[p] b)) ->
  Choreography univ ps' (Faced univ ps b)
foreach (Faced facedImpl) choreoFunc = case facedImpl of
  FacedNil                    -> sub $ CFG.pure $ Faced $ FacedNil
  FacedCons located facedTail -> CFG.do
    newLocated <- choreoFunc located
    newFacedTail <- foreach (Faced facedTail) choreoFunc
    let Faced newFacedImplTail = newFacedTail
    sub $ CFG.pure $ Faced $ FacedCons newLocated newFacedImplTail
      :: Choreography univ ps' (Faced univ ps b)

-- | Collect all values from a Faced value into a Located list
-- collect ::
--   forall univ ps a.
--   (TS.Union ps ps ~ ps, Communicatable a) =>
--   Faced univ ps a ->
--   Choreography univ ps ([] a)
-- collect (Faced facedImpl) = case facedImpl of
--   FacedNil                    -> sub $ CFG.pure []
--   FacedCons (located :: Located univ '[p] a) facedTail -> CFG.do
--     newLocated <- sub $ comm @p located :: Choreography univ ps a
--     newFacedTail <- sub $ collect (Faced facedTail) :: Choreography univ ps ([] a)
--     sub $ CFG.pure $ newLocated : newFacedTail
--       :: Choreography univ ps ([] a)


-- instance P.Functor (Faced univ ps) where
--   fmap f (Faced a) = Faced (f a)
--   fmap _ Unfaced    = Unfaced

-- instance CFG.GradedFunctor (Faced univ) where
--   fmap = P.fmap

-- -- Faced values form a graded monad
-- instance CFG.GradedMonad (Faced univ) where
--   type GMOne (Faced univ) = '[]
--   type GMMul (Faced univ) (x :: [Symbol]) (y :: [Symbol]) = x `TS.Union` y

--   return a = Faced a

--   Faced a >>= f = case f a of
--     Faced b -> Faced b
--     Unfaced -> Unfaced
--   Unfaced >>= _ = Unfaced

-- -- Support for subset operations
-- instance CFG.SubGradable (Faced univ) where
--   type Sub (Faced univ) (x :: [Symbol]) (y :: [Symbol]) = IsSubset x y ~ 'True

--   sub (Faced a) = Faced a
--   sub Unfaced   = Unfaced
