{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module Control.Monad.Free.Linear where
{-
import           Control.Functor.Unrestricted (Applicative (liftA2, pure),
                                         Functor (..), Monad ((>>=)))
import qualified Data.Functor.Linear    as DFL
import           Prelude.Linear         (($), (.))

data LFree f a where
  LPure :: a %1 -> LFree f a
  LBind :: (f b) %1 -> (b %1 -> LFree f a) %1 -> LFree f a

instance DFL.Functor (LFree f) where
  fmap f fa = case fa of
    LPure a    -> LPure $ f a
    LBind fb g -> LBind fb (DFL.fmap f . g)

instance Functor (LFree f) where
  fmap f fa = case fa of
    LPure a    -> LPure $ f a
    LBind fb g -> LBind fb (fmap f . g)

instance DFL.Applicative (LFree f) where
  pure  = LPure
  liftA2 f fa fb = case fa of
    LPure a       -> fmap (f a) fb
    LBind fp cont -> LBind fp $ \p -> DFL.liftA2 f (cont p) fb

instance Applicative (LFree f) where
  pure = LPure
  liftA2 f fa fb = case fa of
    LPure a       -> fmap (f a) fb
    LBind fp cont -> LBind fp $ \p -> liftA2 f (cont p) fb

instance Monad (LFree f) where
  fa >>= cont = case fa of
    LPure a        -> cont a
    LBind fp cont' -> LBind fp $ \p -> cont' p >>= cont

liftF :: f a %1 -> LFree f a
liftF fa = LBind fa LPure

interpretF :: (forall b. f b %1 -> (b %1 -> a) %1 -> a) -> LFree f a %1 -> a
interpretF handler = \case
  LPure a -> a
  LBind fp cont -> handler fp (interpretF handler . cont)
-}
