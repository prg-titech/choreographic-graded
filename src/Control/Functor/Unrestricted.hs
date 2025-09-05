{-# LANGUAGE LinearTypes #-}

module Control.Functor.Unrestricted where
import           Data.Unrestricted.Linear (Ur)

class UrFunctor f where
  map :: (a -> Ur b) %1 -> f a %1 -> f b

class UrFunctor f => UrMonad f where
  pure :: a -> f a
  bind :: f a %1 -> (a -> f b) %1 -> f b

infixl 4 >>=

(>>=) :: (UrMonad f) => f a %1 -> (a -> f b) %1 -> f b
(>>=) = bind
