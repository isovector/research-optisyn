{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GenType
  ( toBurstDecl
  , ToBurstType(..)
  , Marshall(..)
  , typeName
  , necessaryTypes
  , GNecessaryTypes
  ) where

import qualified Data.Set as S
import Data.Set (Set)
import GHC.Generics hiding (conName)
import Types
import Data.Kind (Constraint)
import qualified Data.Kind as K
import GHC.TypeLits hiding (Nat)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)
import ShowType (ShowTypeSym, CanShowType)


data Nat = O | S Nat
  deriving (Eq, Ord, Show, Generic)

instance (CanShowType a, CanShowType b, CanShowType (a , b), Marshall a, Marshall b) => Marshall (a, b)

instance Marshall Nat

instance (CanShowType a, CanShowType [a], Marshall a) => Marshall [a]

instance Marshall Bool
instance Marshall ()

class ToBurstType a where
  toBurstType :: Type

  default toBurstType :: (GToBurstType (Rep a)) => Type
  toBurstType = gtoBurstType @(Rep a)

class (GToBurstDecl a (Rep a)) => Marshall a where
  marshall :: a -> Expr

  default marshall :: (Generic a, GMarshall a (Rep a)) => a -> Expr
  marshall = gmarshall @a . from


toBurstDecl :: forall a. (GToBurstDecl a (Rep a)) => TyDecl
toBurstDecl = gtoBurstDecl @a @(Rep a)


type GMarshall :: K.Type -> (K.Type -> K.Type) -> Constraint
class GMarshall a f where
  gmarshall :: f x -> Expr

type GToBurstDecl :: K.Type -> (K.Type -> K.Type) -> Constraint
class GToBurstDecl a f where
  gtoBurstDecl :: TyDecl

instance (CanShowType a, GToBurstCons a f) => GToBurstDecl a (D1 ('MetaData nm _1 _2 _3) f) where
  gtoBurstDecl = TyDecl (typeName @a) $ gtoBurstCons @a @f


mktypeName :: forall nm. (KnownSymbol nm) => Text
mktypeName = T.pack $
  case lowerFirst $ symbolVal $ Proxy @nm of
    "[]" -> "IMPOSSIBLE?"
    "(,)" -> "IMPOSSIBLE?"
    ":*:" -> "IMPOSSIBLE?"
    t -> t


typeName :: forall a nm. (nm ~ ShowTypeSym a, KnownSymbol nm) =>Text
typeName = mktypeName @nm

conName :: forall ty nm. (KnownSymbol ty, KnownSymbol nm) => Text
conName = T.pack $ mappend (symbolVal (Proxy @ty) <> "_") $
  case symbolVal $ Proxy @nm of
    "[]" -> "Nil"
    ":" -> "Cons"
    "(,)" -> "Pair"
    "()" -> "Unit"
    t -> t

type GToBurstCons :: K.Type -> (K.Type -> K.Type) -> Constraint
class GToBurstCons a f where
  gtoBurstCons :: [DataCon]

instance (ty ~ ShowTypeSym a, KnownSymbol ty, KnownSymbol nm, GToBurstType f) => GToBurstCons a (C1 ('MetaCons nm _1 _2) f) where
  gtoBurstCons = pure $ DataCon (conName @ty @nm) (fromSym @nm) (gtoBurstType @f)

fromSym :: forall nm. KnownSymbol nm => Text
fromSym = T.pack $ symbolVal $ Proxy @nm

instance (GToBurstCons a f, GToBurstCons a g) => GToBurstCons a (f :+: g) where
  gtoBurstCons = gtoBurstCons @a @f ++ gtoBurstCons @a @g

type GToBurstType :: (K.Type -> K.Type) -> Constraint
class GToBurstType f where
  gtoBurstType :: Type

instance (nm ~ ShowTypeSym a, KnownSymbol nm) => GToBurstType (K1 _1 a) where
  gtoBurstType = TyVar $ typeName @a




lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (a : as) = toLower a : as

instance Marshall a => GMarshall ty (K1 _1 a) where
  gmarshall (K1 a) = marshall a

instance (GToBurstType f, GToBurstType g) => GToBurstType (f :*: g) where
  gtoBurstType = TyTuple (gtoBurstType @f) (gtoBurstType @g)

instance (GMarshall a f, GMarshall a g) => GMarshall a (f :*: g) where
  gmarshall (a :*: b) = Tuple (gmarshall @a a) (gmarshall @a b)


instance (GToBurstType f) => GToBurstType (M1 _1 _2 f) where
  gtoBurstType = gtoBurstType @f

instance (GMarshall a f) => GMarshall a (M1 _1 _2 f) where
  gmarshall (M1 a) = gmarshall @a a

instance {-# OVERLAPPING #-} (ty ~ ShowTypeSym a, KnownSymbol ty, KnownSymbol nm, GMarshall a f) => GMarshall a (M1 C ('MetaCons nm _1 _2) f) where
  gmarshall (M1 a) = Apply (Var $ conName @ty @nm) $ gmarshall @a a

instance {-# OVERLAPPING #-} (ty ~ ShowTypeSym a, KnownSymbol ty, KnownSymbol nm) => GMarshall a (M1 C ('MetaCons nm _1 _2) U1) where
  gmarshall (M1 _) = Var $ conName @ty @nm

instance (GMarshall a f, GMarshall a g) => GMarshall a (f :+: g) where
  gmarshall (L1 a) = gmarshall @a a
  gmarshall (R1 a) = gmarshall @a a

instance GToBurstType U1 where
  gtoBurstType = TyUnit

instance GMarshall a U1 where
  gmarshall _ = Unit

necessaryTypes
    :: forall a. (GNecessaryTypes (Rep a), GToBurstDecl a (Rep a))
    => Set TyDecl
necessaryTypes = goNecessaryTypes @a mempty

goNecessaryTypes
    :: forall a. (GNecessaryTypes (Rep a), GToBurstDecl a (Rep a))
    => Set TyDecl
    -> Set TyDecl
goNecessaryTypes = gnecessaryTypes @(Rep a) . S.insert (toBurstDecl @a)


type GNecessaryTypes :: (K.Type -> K.Type) -> Constraint
class GNecessaryTypes f where
  gnecessaryTypes :: Set TyDecl -> Set TyDecl

instance GNecessaryTypes f => GNecessaryTypes (M1 _1 _2 f) where
  gnecessaryTypes = gnecessaryTypes @f

instance (GNecessaryTypes f, GNecessaryTypes g) => GNecessaryTypes (f :*: g) where
  gnecessaryTypes = gnecessaryTypes @f . gnecessaryTypes @g

instance (GNecessaryTypes f, GNecessaryTypes g) => GNecessaryTypes (f :+: g) where
  gnecessaryTypes = gnecessaryTypes @f . gnecessaryTypes @g

instance GNecessaryTypes U1 where
  gnecessaryTypes = id

instance GNecessaryTypes V1 where
  gnecessaryTypes = id

instance (GToBurstDecl a (Rep a), GNecessaryTypes (Rep a)) => GNecessaryTypes (K1 _1 a) where
  gnecessaryTypes t =
    let def = toBurstDecl @a
     in case S.member def t of
          False -> goNecessaryTypes @a $ S.insert def t
          True -> t

