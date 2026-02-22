{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Choreographic.Graded.Location where

import           Data.Proxy     (Proxy (Proxy))
import           Data.Type.Bool (If)
import           Data.Type.Set  (AsSet)
import qualified Data.Type.Set  as TS
import           GHC.Base       (Constraint, Symbol, Type)
import           GHC.TypeLits   (CmpSymbol, KnownSymbol, symbolVal)

type instance TS.Cmp (s1 :: Symbol) (s2 :: Symbol) = CmpSymbol s1 s2

-- class AllKnownSymbols (xs :: [] Symbol) where
--   allKnownSymbols :: Proxy xs -> [] String

-- instance AllKnownSymbols '[] where
--   allKnownSymbols _ = []

-- instance (KnownSymbol x, AllKnownSymbols xs) => AllKnownSymbols (x ': xs) where
--   allKnownSymbols _ =
--     symbolVal (Proxy @x) : allKnownSymbols (Proxy @xs)

class All (f :: k1 -> Constraint) (xs :: [] k1) where
  unconsWithAll
    :: forall r
    . Proxy r
    -> Proxy f
    -> Proxy xs
    -> (forall y ys. (f y, All f ys, IsSubset '[y] xs ~ True, IsSubset ys xs ~ True) => Proxy y -> Proxy ys -> r (y : ys))
    -> r '[] -- Nil
    -> r xs
  unconsConstWithAll
    :: forall r
    .  Proxy f
    -> Proxy xs
    -> (forall y ys. (f y, All f ys, IsSubset '[y] xs ~ True, IsSubset ys xs ~ True) => Proxy y -> Proxy ys -> r)
    -> r
    -> r

instance All f '[] where
  unconsWithAll _ _ _ _ rNil = rNil
  unconsConstWithAll _ _ _ rNil = rNil

instance (All f xs, f x) => All f (x : xs) where
  unconsWithAll _ _ _ f _
    = f (Proxy @x) (Proxy @xs)
  unconsConstWithAll _ _ f _
    = f (Proxy @x) (Proxy @xs)

type family ConstString x :: Type where
  ConstString x = String

class (All KnownSymbol xs) => AllKnownSymbols xs

instance All KnownSymbol xs => AllKnownSymbols xs

allKnownSymbols :: forall xs. AllKnownSymbols xs => [] String
allKnownSymbols =
  unconsConstWithAll (Proxy @KnownSymbol) (Proxy @xs)
    (\prxY (_ :: Proxy ys) -> symbolVal prxY : allKnownSymbols @ys)
    []

type family IsMember (p :: u) (ps :: [] u) :: Bool where
  IsMember p '[] = 'False
  IsMember p (p ': ps) = 'True
  IsMember p (q ': ps) = IsMember p ps

type family Intersect (xs :: [] k1) (ys :: [] k1) :: [] k1 where
  Intersect '[] ys = '[]
  Intersect xs '[] = '[]
  Intersect (x ': xs) ys =
    If (IsMember x ys) (x ': Intersect xs ys) (Intersect xs ys)

type family IsSubset (xs :: [] k1) (ys :: [] k1) :: Bool where
  IsSubset '[] ys = 'True
  IsSubset xs (x ': xs) = 'True
  IsSubset '[x] (x ': xs) = 'True
  IsSubset (x ': xs) ys =
    If (IsMember x ys) (IsSubset xs ys) 'False
  IsSubset xs '[] = 'False

type family MapList (f :: k1 -> k2) (xs :: [] k1) :: [k2] where
  MapList _ '[] = '[]
  MapList f (x ': xs) = f x ': MapList f xs

type family MapSet (f :: k1 -> k2) (xs :: [] k1) :: [k2] where
  MapSet f xs = AsSet (MapList f xs)
