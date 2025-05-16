{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Functor.Graded.Linear where
import GHC.Base (Type)
import qualified Data.Grade as G

class GradedFunctor f where
  fmap :: (a %1 -> b) %1 -> f x a %1 -> f x b

class (GradedFunctor f) => GradedMonad (f :: g -> Type -> Type) where
  return :: a %1 -> f (G.One g) a
  (>>=) :: f x a %1 -> (a %1 -> f y b) %1 -> f (G.Mul g x y) b

class (GradedFunctor f) => GradedSubMonad (f :: g -> Type -> Type) where
  sub :: G.Sub g x y => f x a %1 -> f y a

class (GradedMonad f, GradedSubMonad f) => Effect f
