{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Choreographic.Graded.AST where

import           Data.Kind
import           Data.Proxy     (Proxy (Proxy))
import           Data.Type.Bool (If)
import qualified Data.Type.Map  as TM
import qualified Data.Type.Set  as TS
import           GHC.TypeLits

class Communicatable a where
  serialize :: a -> String
  deserialize :: String -> a

class TypeSet u where
  type EmptySet u :: u
  type AllSet u :: u
  type UnionSet u (x :: u) (y :: u) :: u
  type IntersectSet u (x :: u) (y :: u) :: u
  type Subset u (x :: u) (y :: u) :: Constraint

  type Singleton u (x :: k) :: u

class CoeffectGrade u where
  type OneCoeffect u :: u
  type MultCoeffect u (a :: u) (b :: u) :: u
  type SubsCoeffect u (a :: u) (b :: u) :: Constraint
  type ZeroCoeffect u :: u
  type AddCoeffect u (a :: u) (b :: u) :: u

class EffectGrade u where
  type OneEffect u :: u
  type MultEffect u (a :: u) (b :: u) :: u
  type SubsEffect u (a :: u) (b :: u) :: Constraint

instance (TypeSet u) => CoeffectGrade u where
  type OneCoeffect u = AllSet u
  type MultCoeffect u a b = IntersectSet u a b
  type SubsCoeffect u a b = Subset u a b
  type ZeroCoeffect u = EmptySet u
  type AddCoeffect u a b = UnionSet u a b

instance (TypeSet u) => EffectGrade u where
  type OneEffect u = EmptySet u
  type MultEffect u a b = UnionSet u a b
  type SubsEffect u a b = Subset u a b

data Func u (a :: Type) (p :: u) (b :: Type) (q :: u)

type family MultCoeffectAll (u :: Type) (a :: u) (bs :: [] (TM.Mapping Symbol u)) :: [] (TM.Mapping Symbol u) where
  MultCoeffectAll u a '[] = '[]
  MultCoeffectAll u a ((k TM.:-> b) ': xs) =
    (k TM.:-> MultCoeffect u a b) ': MultCoeffectAll u a xs

type family DeleteMapByKey (key :: k) (m :: [] (TM.Mapping k v)) :: [] (TM.Mapping k v) where
  DeleteMapByKey key '[] = '[]
  DeleteMapByKey key ((key TM.:-> v) ': xs) = DeleteMapByKey key xs
  DeleteMapByKey key ((k' TM.:-> v) ': xs) = (k' TM.:-> v) ': DeleteMapByKey key xs

type family Disjoint (a :: [] (TM.Mapping k v)) (b :: [] (TM.Mapping k v)) :: Constraint where
  Disjoint '[] '[] = ()
  Disjoint '[] _ = ()
  Disjoint _ '[] = ()
  Disjoint ((k TM.:-> v) ': xs) ((k TM.:-> v) ': ys) = TypeError ('Text "Duplicate key: " ':<>: 'ShowType k)
  Disjoint ((k TM.:-> v) ': xs) ((k' TM.:-> v') ': ys) = Disjoint xs ys

data
  Val
    (u :: Type)
    (contexts :: [] (TM.Mapping Symbol Type))
    (coeffects :: [] (TM.Mapping Symbol u))
    (result :: Type)
  where
  Const :: forall u a. a -> Val u '[] '[] a
  Var :: forall u x a. (CoeffectGrade u) => Val u '[x TM.:-> a] '[x TM.:-> OneCoeffect u] a
  Lam ::
    forall u ctxs cefs x a p b q.
    (CoeffectGrade u, TM.Member x ctxs ~ True, TM.Member x cefs ~ True) =>
    Proxy x ->
    Cmp u ctxs cefs b q ->
    Val u (DeleteMapByKey x ctxs) (DeleteMapByKey x cefs) (Func u a p b q)

data
  Cmp
    (u :: Type)
    (contexts :: [] (TM.Mapping Symbol Type))
    (coeffects :: [] (TM.Mapping Symbol u))
    (result :: Type)
    (effect :: u)
  where
  Return ::
    forall u ctxs cefs a.
    (EffectGrade u) =>
    Val u ctxs cefs a ->
    Cmp u ctxs cefs a (OneEffect u)
  App ::
    forall u ctxs1 cefs1 ctxs2 cefs2 a b p q.
    (CoeffectGrade u, Disjoint ctxs1 ctxs2, Disjoint cefs1 cefs2) =>
    Val u ctxs1 cefs1 (Func u a p b q) ->
    Val u ctxs2 cefs2 a ->
    Cmp u (ctxs1 `TM.Union` ctxs2) (cefs1 `TM.Union` MultCoeffectAll u p cefs2) b q
  Do ::
    forall u ctxs1 cefs1 ctxs2 cefs2 a p1 b p2 x.
    (CoeffectGrade u, EffectGrade u, TM.Member x ctxs2 ~ True, TM.Member x cefs2 ~ True, Disjoint ctxs1 ctxs2, Disjoint cefs1 cefs2) =>
    Proxy x ->
    Cmp u ctxs1 cefs1 a p1 ->
    Cmp u ctxs2 cefs2 b p2 ->
    Cmp
      u
      (ctxs1 `TM.Union` DeleteMapByKey x ctxs2)
      (cefs1 `TM.Union` DeleteMapByKey x cefs2)
      b
      (MultEffect u p1 p2)
  Enclave ::
    forall u ctxs1 cefs1 ctxs2 cefs2 a p1 b p2 x p3 pTarget.
    ( CoeffectGrade u
    , EffectGrade u
    , TM.Member x ctxs2 ~ True
    , TM.Member x cefs2 ~ True
    , TM.Lookup cefs2 x ~ 'Just p3
    , SubsCoeffect u p3 pTarget
    , SubsEffect u p1 pTarget
    , Disjoint ctxs1 ctxs2
    , Disjoint cefs1 cefs2
    ) =>
    Proxy pTarget ->
    Proxy x ->
    Cmp u ctxs1 cefs1 a p1 ->
    Cmp u ctxs2 cefs2 b p2 ->
    Cmp
      u
      (ctxs1 `TM.Union` DeleteMapByKey x ctxs2)
      (cefs1 `TM.Union` DeleteMapByKey x (MultCoeffectAll u pTarget cefs2))
      b
      (MultEffect u pTarget p2)
  Comm ::
    forall u ctxs cefs a x.
    (Communicatable a, TypeSet u) =>
    Proxy x ->
    Val u ctxs cefs a ->
    Cmp
      u
      ctxs
      (MultCoeffectAll u (Singleton u x) cefs)
      a
      (Singleton u x)
  Contr :: -- Split (x -> y, z)
    forall u ctxs cefs x y z a b p1 p2 pe.
    ( CoeffectGrade u
    , TM.Member y ctxs ~ True
    , TM.Member y cefs ~ True
    , TM.Member z ctxs ~ True
    , TM.Member z cefs ~ True
    , TM.Lookup ctxs y ~ 'Just a
    , TM.Lookup cefs y ~ 'Just p1
    , TM.Lookup ctxs z ~ 'Just a
    , TM.Lookup cefs z ~ 'Just p2
    , TM.Member x ctxs ~ False
    , TM.Member x cefs ~ False
    , TM.Member y ctxs ~ True
    , TM.Member y cefs ~ True
    ) =>
    Proxy x ->
    Proxy y ->
    Proxy z ->
    Cmp u ctxs cefs b pe ->
    Cmp
      u
      (TM.AsMap ((x TM.:-> a) : DeleteMapByKey y (DeleteMapByKey z ctxs)))
      (TM.AsMap ((x TM.:-> AddCoeffect u p1 p2) : DeleteMapByKey y (DeleteMapByKey z cefs)))
      b
      pe
  Weak :: -- Ignore
    forall u ctxs cefs x a b pe.
    ( CoeffectGrade u
    ) =>
    Proxy x ->
    Cmp u ctxs cefs b pe ->
    Cmp
      u
      (TM.AsMap ((x TM.:-> a) : ctxs))
      (TM.AsMap ((x TM.:-> ZeroCoeffect u) : cefs))
      b
      pe

class AllKnownSymbols (xs :: [] Symbol) where
  allSymbolVal :: Proxy xs -> [] String

instance AllKnownSymbols '[] where
  allSymbolVal _ = []

instance (KnownSymbol x, AllKnownSymbols xs) => AllKnownSymbols (x ': xs) where
  allSymbolVal _ =
    symbolVal (Proxy @x) : allSymbolVal (Proxy @xs)

data TEndpoints (univ :: [] Symbol)
  = forall (xs :: [] Symbol).
    TSet (Proxy univ) (Proxy xs)

-- instance Show (TEndpoints univ) where
--   show (TSet proxUniv proxXs) =
--     "TSet " ++ show (allSymbolVal proxUniv) ++ " " ++ show (allSymbolVal proxXs)

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

instance (AllKnownSymbols univ) => TypeSet (TEndpoints univ) where
  type EmptySet (TEndpoints univ) = 'TSet ('Proxy :: Proxy univ) ('Proxy :: Proxy '[])
  type AllSet (TEndpoints univ) = 'TSet ('Proxy :: Proxy univ) ('Proxy :: Proxy univ)
  type
    UnionSet (TEndpoints univ) (TSet _ ('Proxy :: Proxy xs)) (TSet _ ('Proxy :: Proxy ys)) =
      'TSet ('Proxy :: Proxy univ) ('Proxy :: Proxy (xs TS.:++ ys))
  type
    IntersectSet (TEndpoints univ) (TSet _ ('Proxy :: Proxy xs)) (TSet _ ('Proxy :: Proxy ys)) =
      'TSet ('Proxy :: Proxy univ) ('Proxy :: Proxy (Intersect xs ys))
  type
    Subset (TEndpoints univ) (TSet _ ('Proxy :: Proxy xs)) (TSet _ ('Proxy :: Proxy ys)) =
      IsSubset xs ys ~ 'True
  type
    Singleton (TEndpoints univ) (x :: Symbol) =
      'TSet ('Proxy :: Proxy univ) ('Proxy :: Proxy '[x])

data
  ValEvaluation
    (contexts :: [] (TM.Mapping Symbol Type))
    (result :: Type)
  where
  ValEvaluation ::
    forall contexts result.
    (TM.IsMap contexts) =>
    (TM.Map contexts -> result) ->
    ValEvaluation contexts result

-- data
--   CmpEvaluation
--     (contexts :: [] (TM.Mapping Symbol Type))
--     (result :: Type)
--   where
--   CmpEvaluation ::
--     forall contexts result.
--     (TM.IsMap contexts) =>
--     (TM.Map contexts -> Process result) ->
--     CmpEvaluation contexts result

-- valEval ::
--   forall u contexts cefs result.
--   (EffectGrade u, CoeffectGrade u) =>
--   Val u contexts cefs result ->
--   ValEvaluation contexts result
-- valEval = undefined

-- cmpEval ::
--   forall u contexts cefs result eff.
--   (EffectGrade u, CoeffectGrade u) =>
--   Cmp u contexts cefs result eff ->
--   CmpEvaluation contexts result
-- cmpEval = undefined
