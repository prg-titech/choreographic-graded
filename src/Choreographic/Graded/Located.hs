{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Choreographic.Graded.Located where

import Choreographic.Graded.Location (Intersect, IsSubset)
import qualified Control.Coeffect as Coeffect
import Data.Type.Set (IsSet)
import qualified Data.Type.Set as TS
import GHC.TypeLits (Symbol)

{- | A 'Choreography' is a type of process that can be used to describe
  the behavior of a destributed system. It is parameterized by
  a set of endpoints and a type of actions.
-}
data Located (univ :: [] Symbol) (ps :: [] Symbol) a
  = Located a
  | Unlocated

instance Functor (Located univ ps) where
  fmap f (Located x) = Located (f x)
  fmap _ Unlocated = Unlocated

instance Coeffect.Coeffect (Located univ) where
  extract (Located x) = x
  extract Unlocated = error "Unlocated value cannot be extracted"
  extend f (Located x) = Located (f (Located x))
  extend _ Unlocated = Unlocated

  type Inv (Located univ) f g = (TS.IsSet f, TS.IsSet g)
  type Unit (Located univ) = univ
  type Plus (Located univ) f g = (Intersect f g)

instance (IsSubset ps ps' ~ True) => Coeffect.Subcoeffect (Located univ) ps' ps where
  subco (Located x) = Located x
  subco Unlocated = Unlocated
