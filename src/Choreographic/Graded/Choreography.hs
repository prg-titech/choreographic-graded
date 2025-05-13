{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Choreographic.Graded.Choreography where

import Choreographic.Graded.Location (IsSubset, SingletonSymbolSet, singletonSymbolSetToList)
import Choreographic.Graded.Process (Process)
import qualified Control.Effect as Effect
import Control.Effect.Writer (Symbol)
import Data.Type.Set (IsSet)
import qualified Data.Type.Set as TS

{- | A 'Choreography' is a type of process that can be used to describe
  the behavior of a destributed system. It is parameterized by
  a set of endpoints and a type of actions.
-}
data Choreography (univ :: [] Symbol) (ps :: [] Symbol) a
  = Choreography
      ( forall.
        -- \| 簡単のためこれらのプロパティは常に満たすものとみなす。Choreography コンストラクタを直接使用しなければ問題ない。
        -- \| 使えるのは Effect / Coeffect インスタンスから導出されるされるされる関数、comm および enclave
        -- (IsSubset ps ps' ~ True, IsSubset ps' univ ~ True, IsMember p ps' ~ True) =>
        [] String ->
        String ->
        Process a
      )

instance Functor (Choreography univ ps) where
  fmap f (Choreography g) = Choreography \symPs symP -> do
    x <- g symPs symP
    return (f x)

instance Effect.Effect (Choreography univ) where
  return x = Choreography $ \_ _ -> return x
  (Choreography g) >>= f = Choreography \symPs symP -> do
    x <- g symPs symP
    let (Choreography h) = f x
    h symPs symP

  type Inv (Choreography univ) f g = (TS.IsSet f, TS.IsSet g)
  type Unit (Choreography univ) = '[]
  type Plus (Choreography univ) f g = (f TS.:++ g)

instance (IsSet ps, IsSet ps', IsSubset ps ps' ~ True, IsSubset ps' univ ~ True) => Effect.Subeffect (Choreography univ) ps ps' where
  sub (Choreography g) = Choreography g

-- | Run a Choreography in endopoint p
runChoreography ::
  forall univ a.
  SingletonSymbolSet univ ->
  Choreography univ univ a ->
  String ->
  Process a
runChoreography univSym (Choreography g) p
  | p `elem` singletonSymbolSetToList univSym =
      g (singletonSymbolSetToList univSym) p
runChoreography _ _ _ =
  error "runChoreography: p is not a member of univ"