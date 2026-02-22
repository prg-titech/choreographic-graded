{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QualifiedDo      #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Choreographic.Graded          (Choreography,
                                                CommunicationHooks (..))
import qualified Choreographic.Graded          as C
import           Choreographic.Graded.Location ()
import           Control.Functor.Graded        (GradedComonadApply (new))
import qualified Control.Functor.Graded        as CFG
import qualified Data.Map                      as Map
import qualified Data.Type.Set                 as TS

type Univ = TS.AsSet '["a", "b", "c"]

type A = '["a"]
type B = '["b"]
type C = '["c"]

-- Conclave containing only locations "a" and "b"
type ConclaveAB = TS.AsSet '["a", "b"]

-- Inner choreography that runs only between a and b
innerChoreography :: Choreography Univ ConclaveAB Int
innerChoreography = CFG.sub $ CFG.do
  let
    -- a has a secret number
    aSecret :: C.Located Univ A Int
    aSecret = new 42

    -- b has a multiplier
    bMultiplier :: C.Located Univ B Int
    bMultiplier = new 3

  -- a sends secret to b
  secret <- C.comm @"a" aSecret
  let
    -- b computes result
    bResult :: C.Located Univ B Int
    bResult = (*) CFG.<$> bMultiplier CFG.<@> secret
  -- b sends result back to a
  C.comm @"b" bResult

-- Main program that uses conclave
program :: Choreography Univ Univ Int
program = CFG.sub $ CFG.do
  let
    -- Located choreography at locations a and b
    locatedInnerChor :: C.Located Univ ConclaveAB (Choreography Univ ConclaveAB Int)
    locatedInnerChor = new innerChoreography

  -- Run the conclave (only a and b participate)
  conclaveResult <- C.conclave locatedInnerChor

  let
    -- c has an adder value
    cAdder :: C.Located Univ C Int
    cAdder = new 10

  -- Extract result from conclave and send to all locations
  result <- C.comm @"a" conclaveResult  -- Assuming result is at location "a"

  let
    -- c adds its value to the conclave result
    cFinalResult :: C.Located Univ C Int
    cFinalResult = (+) CFG.<$> cAdder CFG.<@> result

  -- c sends final result to all
  C.comm @"c" cFinalResult

main :: IO ()
main = do
  putStrLn "=== Running Conclave Demo with Communication Logging ==="
  putStrLn "Expected: Location 'c' should NOT see the secret value '42'"
  putStrLn "The secret '42' should only be communicated between 'a' and 'b' inside the conclave"
  putStrLn ""

  let hooks = CommunicationHooks
        { onSend = \from to value ->
            putStrLn $ "[SEND] " ++ from ++ " -> " ++ to ++ " | Value: " ++ value
        , onReceive = \from to value ->
            putStrLn $ "[RECV] " ++ from ++ " -> " ++ to ++ " | Value: " ++ value
        }

  finalMap <- C.runChoreographyConcurrentWithHooks hooks program

  putStrLn ""
  putStrLn "=== Final Results ==="
  print ("Final result at a:", Map.lookup "a" finalMap)
  print ("Final result at b:", Map.lookup "b" finalMap)
  print ("Final result at c:", Map.lookup "c" finalMap)

  putStrLn ""
  putStrLn "=== Verification ==="
  putStrLn "If conclave works correctly:"
  putStrLn "- You should see '42' being sent from 'a' to 'b'"
  putStrLn "- You should see '126' (42*3) being sent from 'b' to 'a'"
  putStrLn "- Location 'c' should NEVER receive or send '42'"
  putStrLn "- Location 'c' should only see the final result '136' (126+10)"
