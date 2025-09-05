{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Choreographic.Graded.Faced where

import           Choreographic.Graded.Location (IsSubset)
import           Control.Functor.Graded        (GradedFunctor (..),
                                                GradedMonad (..))
import qualified Control.Functor.Graded        as CFG
import qualified Data.Type.Set                 as TS
import           GHC.Exts                      (Symbol)
import           Prelude                       (Bool (..), Eq (..), Show (..))
import qualified Prelude                       as P

-- | Faced values can differ between locations
-- Unlike Located values which are uniform, Faced values
-- represent location-specific data that may be different at each location
data Faced (univ :: [Symbol]) (ps :: [Symbol]) a
  = Faced a      -- ^ Value that can differ per location
  | Unfaced      -- ^ No value
  deriving (Show, Eq)

instance P.Functor (Faced univ ps) where
  fmap f (Faced a) = Faced (f a)
  fmap _ Unfaced    = Unfaced

instance CFG.GradedFunctor (Faced univ) where
  fmap = P.fmap

-- Faced values form a graded monad  
instance CFG.GradedMonad (Faced univ) where
  type GMOne (Faced univ) = '[]
  type GMMul (Faced univ) (x :: [Symbol]) (y :: [Symbol]) = x `TS.Union` y

  return a = Faced a

  Faced a >>= f = case f a of
    Faced b -> Faced b
    Unfaced -> Unfaced
  Unfaced >>= _ = Unfaced

-- Support for subset operations
instance CFG.SubGradable (Faced univ) where
  type Sub (Faced univ) (x :: [Symbol]) (y :: [Symbol]) = IsSubset x y ~ 'True
  
  sub (Faced a) = Faced a
  sub Unfaced   = Unfaced