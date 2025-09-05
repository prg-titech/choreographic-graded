{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Functor.Graded.Linear where

import           GHC.Base       (Constraint, Type)
import           Prelude.Linear (uncurry, ($))

class GradedFunctor f where
  gmap :: (a %1 -> b) -> f x a %1 -> f x b

infixl 4 ^>$<

(^>$<) :: (GradedFunctor f) => (a %1 -> b) -> f x a %1 -> f x b
(^>$<) = gmap

class (GradedFunctor f) => GradedComonad (f :: g -> Type -> Type) where
  type ComonadOne f :: g
  type ComonadMul f (x :: g) (y :: g) :: g

  gExtract :: f (ComonadOne f) a %1 -> a
  gDuplicate :: f (ComonadMul f x y) a %1 -> f x (f y a)

class (GradedFunctor f) => GradedSubComonad (f :: g -> Type -> Type) where
  type ComonadSub f (x :: g) (y :: g) :: Constraint

  gSub :: (ComonadSub f x y) => f y a %1 -> f x a

class (GradedComonad f) => GradedCtxComonad (f :: g -> Type -> Type) where
  type ComonadZero f :: g
  type ComonadAdd f (x :: g) (y :: g) :: g

  gIgnore :: f (ComonadZero f) a %1 -> ()
  gSplit :: f (ComonadAdd f x y) a %1 -> (f x a, f y a)

infixl 4 ^-<=

(^-<=) :: (GradedCtxComonad f) => f (ComonadAdd f x y) a %1 -> ((f x a, f y a) %1 -> b) %1 -> b
x ^-<= f = f $ gSplit x
class (GradedFunctor f) => GradedApply (f :: g -> Type -> Type) where
  gZip :: (f x a, f x b) %1 -> f x (a, b)
  gUnit :: f x ()

gPure :: (GradedApply f) => a -> f x a
gPure a = gmap (\() -> a) gUnit

gMap2 :: (GradedApply f) => (a %1 -> b %1 -> c) -> f x a %1 -> f x b %1 -> f x c
gMap2 f x y = gmap (uncurry f) $ gZip (x, y)

gApply :: (GradedApply f) => f x (a %1 -> b) %1 -> f x a %1 -> f x b
gApply ff fx = gmap (\(f, x) -> f x) $ gZip (ff, fx)

infixl 4 ^>*<

(^>*<) :: (GradedApply f) => f x (a %1 -> b) %1 -> f x a %1 -> f x b
(^>*<) = gApply

class
  ( GradedComonad f
  , GradedSubComonad f
  , GradedCtxComonad f
  , GradedApply f
  ) =>
  Coeffect f
