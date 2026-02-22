{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QualifiedDo               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Choreographic.Graded.Choreography where

import qualified Choreographic.Graded.Location as Location
import           Control.Applicative           (Applicative (..))
import qualified Control.Functor.Graded        as CFG
import           Control.Monad.Free            (Free, liftF)
import qualified Data.Type.Set                 as TS
import           GHC.Exts                      (Symbol)
import           Prelude                       (Bool (..), Eq, Functor (..),
                                                IO, Ord, Read, Show (..), String,
                                                id, read, ($), (.))

class Communicatable a where
  serialize :: a -> String
  deserialize :: String -> a

instance (Show a, Read a) => Communicatable a where
  serialize = show
  deserialize = read

data CommInfo = CommInfo
  { ciFrom :: String
  , ciTo   :: String
  } deriving (Show, Eq, Ord)

data ProcessOp a where
  -- | Send a message of type 'b'.
  Send :: forall b a. (Communicatable b) => CommInfo -> b -> a -> ProcessOp a
  -- | Receive a message of type 'b'.
  Receive :: forall a b. (Communicatable b) => CommInfo -> (b -> a) -> ProcessOp a
  -- | Perform an IO operation.
  PerformIO :: forall a b. IO a -> (a -> b) -> ProcessOp b

instance Functor ProcessOp where
  fmap f = \case
    Send info content cont -> Send info content $ f cont
    Receive info cont -> Receive info (f . cont)
    PerformIO io cont -> PerformIO io (f . cont)

type Process a = Free ProcessOp a

send :: forall a. (Communicatable a) => CommInfo -> a -> Process ()
send p b = liftF (Send p b ())

receive :: forall a. (Communicatable a) => CommInfo -> Process a
receive p = liftF (Receive p id)

performIO :: forall a. IO a -> Process a
performIO io = liftF (PerformIO io id)


{- | A 'Choreography' is a type of process that can be used to describe
  the behavior of a destributed system. It is parameterized by
  a set of endpoints and a type of actions.
-}
newtype Choreography (univ :: [Symbol]) (ps :: [Symbol]) a
  = Choreography
      (   -- \| 簡単のためこれらのプロパティは常に満たすものとみなす。Choreography コンストラクタを直接使用しなければ問題ない。
          -- \| 使えるのは Effect / Coeffect インスタンスから導出されるされるされる関数、comm および conclave
          -- (IsSubset ps ps' ~ True, IsSubset ps' univ ~ True, IsMember p ps' ~ True) =>
          [] String -> -- Univ
          String -> -- p
          Process a
      )

runChoreography ::
  [String] -> String ->
  Choreography univ ps a ->
  Process a
runChoreography univ p (Choreography mkProcess) = mkProcess univ p

instance CFG.GradedFunctor (Choreography univ) where
  fmap f (Choreography mkProcess) = Choreography $ \univ p -> do
    -- Apply the function to the result of the process
    a <- mkProcess univ p
    pure (f a)

instance CFG.GradedMonad (Choreography univ) where
  type GMOne (Choreography univ) = '[]
  type GMMul (Choreography univ) (x :: [Symbol]) (y :: [Symbol]) = x `TS.Union` y

  return a = Choreography $ \_ _ -> pure a

  (>>=) (Choreography mkProcessA) cont = Choreography $ \univ p -> do
    a <- mkProcessA univ p
    runChoreography univ p (cont a)

instance CFG.SubGradable (Choreography univ) where
  type Sub (Choreography univ) (x :: [Symbol]) (y :: [Symbol]) = Location.IsSubset x y ~ 'True

  sub (Choreography mkProcess) = Choreography $ \univ p -> do
    -- Simply return the process without any change
    mkProcess univ p

instance CFG.Effect (Choreography univ)
