{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Choreographic.Graded.Located where

import           Choreographic.Graded.Location (IsSubset)
import qualified Choreographic.Graded.Location as TS
import qualified Control.Functor.Graded        as CFG
import           GHC.Err                       (error)
import           GHC.TypeLits                  (Symbol)
import           Prelude                       (Bool (..), ($))

{- | A 'Choreography' is a type of process that can be used to describe
  the behavior of a destributed system. It is parameterized by
  a set of endpoints and a type of actions.
-}
data Located (univ :: [] Symbol) (ps :: [] Symbol) a
  = Located a
  | Unlocated

instance CFG.GradedFunctor (Located univ) where
  fmap f (Located x) = Located (f x)
  fmap _ Unlocated   = Unlocated

instance CFG.GradedComonad (Located univ) where
  type GWOne (Located univ) = univ
  type GWMul (Located univ) (x :: [] Symbol) (y :: [] Symbol) = x `TS.Intersect` y

  extract (Located x) = x
  extract Unlocated   = error "Unlocated value cannot be extracted"

  duplicate (Located x) = Located $ Located x
  duplicate Unlocated   = Unlocated

instance CFG.SubGradable (Located univ) where
  type Sub (Located univ) (x :: [] Symbol) (y :: [] Symbol) = IsSubset x y ~ 'True
  sub (Located x) = Located x
  sub Unlocated   = Unlocated

instance CFG.GradedComonadApply (Located univ) where
  apply (Located f) (Located x) = Located $ f x
  apply _ _                     = Unlocated

  new = Located

instance CFG.Coeffect (Located univ)
