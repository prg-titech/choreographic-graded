{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Grade where
import GHC.Base (Constraint)

class MonoidGrade a where
  type One a :: a
  type Mul a :: a -> a -> a

class MonoidGrade a => SemiringGrade a where
  type Zero a :: a
  type Add a :: a -> a -> a

class MonoidGrade a => SubGrade a where
  type Sub a :: a -> a -> Constraint
