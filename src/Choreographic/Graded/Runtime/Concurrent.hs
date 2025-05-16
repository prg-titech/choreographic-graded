{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Choreographic.Graded.Runtime.Concurrent where

import Choreographic.Graded.Choreography (Choreography, runChoreography)
import Choreographic.Graded.Location (SingletonSymbolSet, singletonSymbolSetToList)
import Choreographic.Graded.Process (Communicatable (deserialize, serialize), ProcessOp (Receive, Send))
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forM_)
import Control.Monad.Free (foldFree)
import Data.Map as Map
import Data.Type.Set (IsSet)

handleProcess ::
  forall a.
  Map.Map String (MVar.MVar String) ->
  ProcessOp a ->
  IO a
handleProcess stores op = case op of
  Send to x cont -> do
    Just store <- pure $ stores Map.!? to
    MVar.putMVar store (serialize x)
    pure cont
  Receive from cont -> do
    Just store <- pure $ stores Map.!? from
    x <- MVar.takeMVar store
    pure (cont (deserialize x))

runChoreographyConcurrent ::
  forall univ a.
  (IsSet univ, Communicatable a) =>
  SingletonSymbolSet univ ->
  Choreography univ univ a ->
  IO (Map.Map String a) -- 全部同じになることが期待される
runChoreographyConcurrent univSym choreo = do
  stores <- Map.fromList <$> traverse (\sym -> (sym,) <$> MVar.newEmptyMVar) (singletonSymbolSetToList univSym)
  resultVar <- Map.fromList <$> traverse (\sym -> (sym,) <$> MVar.newEmptyMVar) (singletonSymbolSetToList univSym)
  forM_ (singletonSymbolSetToList univSym) \sym -> do
    forkIO do
      let process = runChoreography univSym choreo sym
      result <- foldFree (handleProcess stores) process
      Just store <- pure $ resultVar Map.!? sym
      MVar.putMVar store (serialize result)
  results <- traverse MVar.takeMVar resultVar
  pure $ fmap deserialize results
