{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Choreographic.Graded.Faceted where

import           Choreographic.Graded.Location (IsSubset)
import           Control.Functor.Graded        (GradedFunctor (..),
                                                GradedMonad (..))
import qualified Control.Functor.Graded        as CFG
import qualified Data.Type.Set                 as TS
import           GHC.Exts                      (Symbol)
import           Prelude                       (Bool (..), Eq (..), Show (..))
import qualified Prelude                       as P

-- | Faceted values can differ between locations
-- Unlike Located values which are uniform, Faceted values
-- represent location-specific data that may be different at each location
data Faceted (univ :: [Symbol]) (ps :: [Symbol]) a
  = Faceted a      -- ^ Value that can differ per location
  | Unfaced      -- ^ No value
  deriving (Show, Eq)

instance P.Functor (Faceted univ ps) where
  fmap f (Faceted a) = Faceted (f a)
  fmap _ Unfaced    = Unfaced

instance CFG.GradedFunctor (Faceted univ) where
  fmap = P.fmap

-- Faceted values form a graded monad
instance CFG.GradedMonad (Faceted univ) where
  type GMOne (Faceted univ) = '[]
  type GMMul (Faceted univ) (x :: [Symbol]) (y :: [Symbol]) = x `TS.Union` y

  return a = Faceted a

  Faceted a >>= f = case f a of
    Faceted b -> Faceted b
    Unfaced -> Unfaced
  Unfaced >>= _ = Unfaced

-- Support for subset operations
instance CFG.SubGradable (Faceted univ) where
  type Sub (Faceted univ) (x :: [Symbol]) (y :: [Symbol]) = IsSubset x y ~ 'True

  sub (Faceted a) = Faceted a
  sub Unfaced   = Unfaced