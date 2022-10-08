{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module ShowType (ShowTypeSym, CanShowType) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind (Type)
import Data.Functor.Identity (Identity)


type CanShowType a = KnownSymbol (ShowTypeSym a)

type RepName :: (Type -> Type) -> Symbol
type family RepName a where
  RepName (M1 D ('MetaData nm _1 _2 _3) x) = nm


type FillHole :: forall k -> k
type family FillHole k where
  FillHole Type = ()
  FillHole (Type -> Type) = Identity


type Fill :: forall k -> k -> Type
type family Fill k f where
  Fill Type a = a
  Fill (k1 -> k2) f = Fill k2 (f (FillHole k1))


type ShowTypeSym :: k -> Symbol
type family ShowTypeSym t where
  ShowTypeSym (t :: k) = ShowType' k t


type ShowType' :: forall k -> k -> Symbol
type family ShowType' k f where
  ShowType' k ((f :: k1 -> k) a) =
    AppendSymbol
      (ShowType' (k1 -> k) f)
      (AppendSymbol "q"
        (AppendSymbol (ShowType' k1 a) "q"))
  ShowType' Type () = "Unit"
  ShowType' Type Int = "Nat"
  ShowType' (Type -> Type) [] = "List"
  ShowType' (Type -> Type -> Type) (,) = "Pair"
  ShowType' k f = RepName (Rep (Fill k f))

