{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Functor.Graded.Linear where

import           GHC.Base (Constraint, Type)

class GradedFunctor f where
  cmap :: (a %1 -> b) %1 -> f x a %1 -> f x b

(!^<$>) :: (GradedFunctor f) => (a %1 -> b) %1 -> f x a %1 -> f x b
(!^<$>) = cmap

class (GradedFunctor f) => GradedMonad (f :: g -> Type -> Type) where
  type EffectOne f :: g
  type EffectMul f (x :: g) (y :: g) :: g

  cUnit :: a %1 -> f (EffectOne f) a
  (!^>>=) :: f x a %1 -> (a %1 -> f y b) %1 -> f (EffectMul f x y) b

cApply :: (GradedMonad f) => f x (a %1 -> b) %1 -> f y a -> f (EffectMul f (x :: g) (y :: g)) b
cApply ff fx = ff !^>>= \f -> f !^<$> fx

(!^<*>) :: (GradedMonad f) => f x (a %1 -> b) %1 -> f y a -> f (EffectMul f (x :: g) (y :: g)) b
(!^<*>) = cApply

class (GradedFunctor f) => GradedSubMonad (f :: g -> Type -> Type) where
  type EffectSub f (x :: g) (y :: g) :: Constraint
  sub :: (EffectSub f x y) => f x a %1 -> f y a

class (GradedMonad f, GradedSubMonad f) => Effect f
