{-# LANGUAGE LinearTypes  #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Functor.Graded.Unrestricted where
import           Data.Kind                (Type)
import           Data.Unrestricted.Linear (Ur)
import           GHC.Types                (Constraint)

class UrGradedFunctor f where
  map :: (a -> Ur b) %1 -> f x a %1 -> f x b

class UrGradedFunctor f => UrGradedMonad f where
  type Zero f :: Type
  type Mul f x y :: Type

  pure :: a -> f (Zero f) a
  bind :: f x a %1 -> (a -> f y b) %1 -> f (Mul f x y) b

class UrGradedSubMonad f where
  type Sub f x y :: Constraint
  sub :: (Sub f x y) => f x a %1 -> f y a

class (UrGradedMonad f, UrGradedSubMonad f) => Effect f


infixl 4 >>=

(>>=) :: (UrGradedMonad f) => f x a %1 -> (a -> f y b) %1 -> f (Mul f x y) b
(>>=) = bind
