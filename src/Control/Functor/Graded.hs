{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-type-equality-requires-operators #-}

module Control.Functor.Graded where
import           GHC.Types (Constraint, Type)

class GradedFunctor f where
  fmap :: (a -> b) -> f x a -> f x b

(<$>) :: (GradedFunctor f) => (a -> b) -> f x a -> f x b
(<$>) = fmap


(<@>) :: (GradedFunctor f) => f x (a -> b) -> a -> f x b
(<@>) fxf a = (\f -> f a) <$> fxf

class (GradedFunctor f) => GradedMonad (f :: g -> Type -> Type) where
  type GMOne f :: g
  type GMMul f (x :: g) (y :: g) :: g

  return :: a -> f (GMOne f) a
  (>>=) :: f x a -> (a -> f y b) -> f (GMMul f x y) b

pure :: GradedMonad f => a -> f (GMOne f) a
pure = return

class (GradedFunctor f) => SubGradable (f :: g -> Type -> Type) where
  type Sub f (x :: g) (y :: g) :: Constraint
  sub :: (Sub f x y) => f x a -> f y a

class (GradedMonad f, SubGradable f) => Effect f

class (GradedFunctor f) => GradedComonad (f :: g -> Type -> Type) where
  type GWOne f :: g
  type GWMul f (x :: g) (y :: g) :: g

  extract :: f (GWOne f) a -> a
  duplicate :: f (GWMul f x y) a -> f x (f y a)

class (GradedComonad f) => GradedComonadApply (f :: g -> Type -> Type) where
  apply :: f x (a -> b) -> f x a -> f x b
  new :: a -> f x a

(<*>) :: (GradedComonadApply f) => f x (a -> b) -> f x a -> f x b
(<*>) = apply

lift2 :: GradedComonadApply f => (a -> b -> c) -> f x a -> f x b -> f x c
lift2 f fxa = apply (fmap f fxa)

class (GradedComonadApply f, SubGradable f) => Coeffect f

forM :: forall f x a b. (Effect f, GMMul f x x ~ x, Sub f (GMOne f) x) => [a] -> (a -> f x b) -> f x [b]
forM [] _       = sub (return [])
forM (a : as) f = f a >>= \b -> forM as f >>= \bs -> (sub (return (b : bs)) :: f x [b])
