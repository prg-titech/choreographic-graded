{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Choreographic.Graded.Operation where

import qualified Choreographic.Graded.Choreography as Choreography
import qualified Choreographic.Graded.Located as Located
import Choreographic.Graded.Location (IsMember, IsSubset, SingletonSymbol, SingletonSymbolSet, singletonSymbolSetToList)
import Choreographic.Graded.Process (Communicatable)
import qualified Choreographic.Graded.Process as Process
import Control.Monad (forM_)
import Data.Type.Set (IsSet)

-- | Communication from a process "p"
comm ::
  forall univ p a.
  (IsSet univ, IsMember p univ ~ 'True) =>
  (Communicatable a) =>
  SingletonSymbol p ->
  Located.Located univ '[p] a ->
  Choreography.Choreography univ '[p] a
comm symP loc =
  Choreography.Choreography \symPs'Str symP'Str -> case loc of
    Located.Located x
      | symP'Str == show symP -> do
          forM_
            symPs'Str
            \symPs'ElemStr ->
              if symPs'ElemStr == show symP
                then pure ()
                else Process.send symPs'ElemStr x -- 実行中コンテキストのコンテキストの自分自身 (symP) 以外の全てのプロセスに送信
          pure x
      | otherwise -> Process.receive $ show symP
    Located.Unlocated
      | symP'Str == show symP -> error "Unlocated value cannot be sent"
      | otherwise -> Process.receive $ show symP

-- | Choreography を ps で実行する
enclave ::
  forall univ ps a.
  (IsSet univ, IsSet ps) =>
  (IsSubset ps univ ~ 'True) =>
  SingletonSymbolSet ps ->
  Located.Located univ ps (Choreography.Choreography univ ps a) ->
  Choreography.Choreography univ ps (Located.Located univ ps a)
enclave symPs loc =
  -- 実行コンテキスト symPs', 現在実行中のプロセス symP'
  -- symP' in symPs'
  -- symPs < symPs'
  -- が保証されているとみて良い
  -- symP' が symPs に含まれているかいないかで分岐
  Choreography.Choreography \_ symP'Str -> case loc of
    Located.Located (Choreography.Choreography f)
      | symP'Str `elem` singletonSymbolSetToList symPs -> do
          res <- f (singletonSymbolSetToList symPs) symP'Str
          pure $ Located.Located res
      | otherwise -> pure Located.Unlocated
    Located.Unlocated
      | symP'Str `elem` singletonSymbolSetToList symPs -> error "Unlocated computation cannot be executed"
      | otherwise -> pure Located.Unlocated
