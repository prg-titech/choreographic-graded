{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Functor.Graded.Linear where

import GHC.Base (Type)
import Prelude.Linear ( uncurry, ($) )
import qualified Data.Grade as G

class GradedFunctor f where
  fmap :: (a %1 -> b) -> f x a %1 -> f x b

class (GradedFunctor f) => GradedComonad (f :: g -> Type -> Type) where
  extract :: f x a %1 -> a
  duplicate :: f x a %1 -> f (G.Mul g x x) (f x a)

class (GradedFunctor f) => GradedSubComonad (f :: g -> Type -> Type) where
  sub :: G.Sub g x y => f y a %1 -> f x a

class (GradedComonad f) => GradedCtxComonad (f :: g -> Type -> Type) where
  ignore :: f (G.Zero g) a %1 -> ()
  split :: f (G.Add g x y) a %1 -> (f x a, f y a)

class (GradedFunctor f) => GradedZipComonad (f :: g -> Type -> Type) where
  zips :: (f x a, f x b) %1 -> f x (a, b)
  gen :: f x ()

class (GradedComonad f, GradedSubComonad f, GradedCtxComonad f, GradedZipComonad f) => Coffect f

gened :: (GradedZipComonad f) => a -> f x a
gened a = fmap (\() -> a) gen

map2 :: (GradedZipComonad f) => (a %1 -> b %1 -> c) -> f x a %1 -> f x b %1 -> f x c
map2 f x y = fmap (uncurry f) $ zips (x, y)

splitThen :: (GradedCtxComonad f) => f (G.Add g x y) a %1 -> ((f x a, f y a) %1 -> b) %1 -> b
splitThen x f = f $ split x
