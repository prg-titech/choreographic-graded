{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Choreographic.Graded.Runtime.Concurrent where

import           Choreographic.Graded.Choreography (Choreography, CommInfo (..),
                                                    Communicatable (..),
                                                    ProcessOp (..),
                                                    runChoreography)
import           Choreographic.Graded.Location     (AllKnownSymbols,
                                                    allKnownSymbols)
import           Control.Concurrent                (forkIO)
import qualified Control.Concurrent.MVar           as MVar
import           Control.Monad                     (forM_)
import           Control.Monad.Free                (foldFree)
import           Data.Map                          as Map
import           Data.Type.Set                     (IsSet)

-- | Hooks for logging send/receive events
data CommunicationHooks = CommunicationHooks
  { onSend    :: String -> String -> String -> IO ()  -- ^ from -> to -> value -> IO ()
  , onReceive :: String -> String -> String -> IO ()  -- ^ from -> to -> value -> IO ()
  }

-- | Default hooks that do nothing
defaultHooks :: CommunicationHooks
defaultHooks = CommunicationHooks
  { onSend = \_ _ _ -> pure ()
  , onReceive = \_ _ _ -> pure ()
  }

handleProcess ::
  forall a.
  CommunicationHooks ->
  Map.Map CommInfo (MVar.MVar String) ->
  ProcessOp a ->
  IO a
handleProcess hooks stores op = case op of
  Send info x cont -> do
    let serializedValue = serialize x
    Just store <- pure $ stores Map.!? info
    onSend hooks (ciFrom info) (ciTo info) serializedValue
    MVar.putMVar store serializedValue
    pure cont
  Receive info cont -> do
    Just store <- pure $ stores Map.!? info
    serializedValue <- MVar.takeMVar store
    onReceive hooks (ciFrom info) (ciTo info) serializedValue
    pure (cont (deserialize serializedValue))
  PerformIO ioAction cont -> cont <$> ioAction

runChoreographyConcurrentWithHooks ::
  forall univ a.
  (IsSet univ, Communicatable a, AllKnownSymbols univ) =>
  CommunicationHooks ->
  Choreography univ univ a ->
  IO (Map.Map String a) -- 全部同じになることが期待される
runChoreographyConcurrentWithHooks hooks choreo = do
  let
    allSyms = allKnownSymbols @univ
  stores <- Map.fromList
    <$> traverse (\sym -> (sym,) <$> MVar.newEmptyMVar)
    [CommInfo { ciTo = sym, ciFrom = sym'} | sym <- allSyms, sym' <- allSyms]
  resultVar <- Map.fromList <$> traverse (\sym -> (sym,) <$> MVar.newEmptyMVar) allSyms
  forM_ allSyms \sym -> do
    forkIO do
      let process = runChoreography allSyms sym choreo
      result <- foldFree (handleProcess hooks stores) process
      Just store <- pure $ resultVar Map.!? sym
      MVar.putMVar store (serialize result)
  results <- traverse MVar.takeMVar resultVar
  pure $ fmap deserialize results

-- | Backward-compatible version using default hooks
runChoreographyConcurrent ::
  forall univ a.
  (IsSet univ, Communicatable a, AllKnownSymbols univ) =>
  Choreography univ univ a ->
  IO (Map.Map String a)
runChoreographyConcurrent = runChoreographyConcurrentWithHooks defaultHooks
