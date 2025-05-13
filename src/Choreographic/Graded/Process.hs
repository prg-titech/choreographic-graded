{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Choreographic.Graded.Process where

import Control.Monad.Free

class Communicatable a where
  serialize :: a -> String
  deserialize :: String -> a

data ProcessOp a where
  -- | Send a message of type 'b' to a process identified by 'p'.
  Send :: forall b a. (Communicatable b) => String -> b -> a -> ProcessOp a
  -- | Receive a message of type 'b' from a process identified by 'p'.
  Receive :: forall a b. (Communicatable b) => String -> (b -> a) -> ProcessOp a

instance Functor ProcessOp where
  fmap f (Send p b x) = Send p b (f x)
  fmap f (Receive p g) = Receive p (f . g)

type Process a = Free ProcessOp a

send :: forall a. (Communicatable a) => String -> a -> Process ()
send p b = liftF (Send p b ())

receive :: forall a. (Communicatable a) => String -> Process a
receive p = liftF (Receive p id)