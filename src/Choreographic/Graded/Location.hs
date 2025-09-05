{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
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
import           GHC.Base       (Symbol)
import           GHC.TypeLits   (CmpSymbol, KnownSymbol, symbolVal,
                                 withKnownSymbol, withSomeSSymbol)

data SingletonSymbol (s :: Symbol) = (KnownSymbol s) => SingletonSymbol (Proxy s)

type instance TS.Cmp (s1 :: Symbol) (s2 :: Symbol) = CmpSymbol s1 s2

singletonSymbol :: forall (s :: Symbol). (KnownSymbol s) => Proxy s -> SingletonSymbol s
singletonSymbol _ = SingletonSymbol (Proxy @s)

instance Show (SingletonSymbol s) where
  show (SingletonSymbol prox) = symbolVal prox

instance Eq (SingletonSymbol s) where
  singSym1 == singSym2 = show singSym1 == show singSym2

class AllKnownSymbols (xs :: [] Symbol) where
  allKnownSymbols :: Proxy xs -> [] String

instance AllKnownSymbols '[] where
  allKnownSymbols _ = []

instance (KnownSymbol x, AllKnownSymbols xs) => AllKnownSymbols (x ': xs) where
  allKnownSymbols _ =
    symbolVal (Proxy @x) : allKnownSymbols (Proxy @xs)

data SingletonSymbolSet (xs :: [] Symbol) = (AllKnownSymbols xs, TS.IsSet xs) => SingletonSymbolSet (Proxy xs)

singletonSymbolSet :: forall (xs :: [] Symbol). (AllKnownSymbols xs, TS.IsSet xs) => Proxy xs -> SingletonSymbolSet xs
singletonSymbolSet _ = SingletonSymbolSet (Proxy @xs)

singletonSymbolSetToList :: SingletonSymbolSet xs -> [] String
singletonSymbolSetToList (SingletonSymbolSet prox) =
  allKnownSymbols prox

isMember ::
  forall (p :: Symbol) (ps :: [] Symbol).
  SingletonSymbol p ->
  SingletonSymbolSet ps ->
  Bool
isMember p ps = show p `elem` singletonSymbolSetToList ps

type family IsMember (p :: u) (ps :: [u]) :: Bool where
  IsMember p '[] = 'False
  IsMember p (p ': ps) = 'True
  IsMember p (q ': ps) = IsMember p ps

type family Intersect (xs :: [] k1) (ys :: [] k2) :: [k1] where
  Intersect '[] ys = '[]
  Intersect xs '[] = '[]
  Intersect (x ': xs) ys =
    If (IsMember x ys) (x ': Intersect xs ys) (Intersect xs ys)

type family IsSubset (xs :: [k1]) (ys :: [k2]) :: Bool where
  IsSubset '[] ys = 'True
  IsSubset xs '[] = 'False
  IsSubset (x ': xs) ys =
    If (IsMember x ys) (IsSubset xs ys) 'False

type family MapList (f :: k1 -> k2) (xs :: [k1]) :: [k2] where
  MapList _ '[] = '[]
  MapList f (x ': xs) = f x ': MapList f xs

type family MapSet (f :: k1 -> k2) (xs :: [k1]) :: [k2] where
  MapSet f xs = AsSet (MapList f xs)
