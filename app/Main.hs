{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Choreographic.Graded          (Choreography)
import qualified Choreographic.Graded          as C
import           Choreographic.Graded.Location ()
import           Control.Functor.Graded        (GradedComonadApply (new))
import qualified Control.Functor.Graded        as CFG
import qualified Data.Map                      as Map
import           Data.Proxy                    (Proxy (Proxy))
import           Data.Type.Bool                (If)
import qualified Data.Type.Set                 as TS
import           GHC.Base                      (Symbol)
import           Prelude                       (Bool (..), IO, Int, Ord (..),
                                                String, print, undefined, ($))

type Univ = TS.AsSet '["a", "b"]

type A = '["a"]

type B = '["b"]

-- a : Seller
-- b : Buyer
program :: Choreography Univ Univ Bool
program = CFG.sub $ CFG.do
  let
    bName :: C.Located Univ B String
    bName = new "Hello"

    bBudget :: C.Located Univ B Int
    bBudget = new 100

    aPriceDict :: C.Located Univ A (String -> Int)
    aPriceDict = new $ \case
          "Hello" -> 0
          _       -> 10000000

  name <- C.comm @"b" bName
  let
    aPrice :: C.Located Univ A Int
    aPrice = aPriceDict CFG.<@> name
  price <- C.comm @"a" aPrice
  let
    bIsBuy :: C.Located Univ B Bool
    bIsBuy = (>) CFG.<$> bBudget CFG.<@> price
  C.comm @"b" bIsBuy

main :: IO ()
main = do
  finalMap <- C.runChoreographyConcurrent @Univ program
  print (Map.lookup "a" finalMap)
