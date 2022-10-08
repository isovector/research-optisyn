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

module GenType (toBurstDecl, BurstRef(..), ToBurstType(..), Marshall(..)) where

import GHC.Generics hiding (conName)
import Types
import Data.Kind (Constraint)
import qualified Data.Kind as K
import GHC.TypeLits hiding (Nat)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)


data Nat = O | S Nat
  deriving (Eq, Ord, Show, Generic)

instance Marshall () where
  marshall () = "U1"
instance BurstRef () where
  burstRef = "u1"

instance (Marshall a, Marshall b) => Marshall (a, b)
instance BurstRef (a, b) where
  burstRef = "builtinpair"

instance Marshall Nat
instance BurstRef Nat

instance (BurstRef a, Marshall a) => Marshall [a]
instance BurstRef [a]

instance Marshall Bool
instance BurstRef Bool

instance BurstRef (U1 a)
instance Marshall (U1 a)

instance BurstRef ((f :+: g) a)
instance (Marshall (f a), Marshall (g a)) => Marshall ((f :+: g) a)

instance BurstRef ((f :*: g) a)
instance (Marshall (f a), Marshall (g a)) => Marshall ((f :*: g) a)

instance BurstRef (K1 _1 _2 a)
instance (Marshall a) => Marshall (K1 _1 a x)

class ToBurstType a where
  toBurstType :: Type

  default toBurstType :: (GToBurstType (Rep a)) => Type
  toBurstType = gtoBurstType @(Rep a)

class (BurstRef a, GToBurstDecl (Rep a)) => Marshall a where
  marshall :: a -> Expr

  default marshall :: (Generic a, GMarshall (Rep a)) => a -> Expr
  marshall = gmarshall . from


toBurstDecl :: forall a. (GToBurstDecl (Rep a)) => TyDecl
toBurstDecl = gtoBurstDecl @(Rep a)


type GMarshall :: (K.Type -> K.Type) -> Constraint
class GMarshall f where
  gmarshall :: f x -> Expr

type GToBurstDecl :: (K.Type -> K.Type) -> Constraint
class GToBurstDecl f where
  gtoBurstDecl :: TyDecl

instance (KnownSymbol nm, GToBurstCons f) => GToBurstDecl (D1 ('MetaData nm _1 _2 _3) f) where
  gtoBurstDecl = TyDecl (typeName @nm) $ gtoBurstCons @f


typeName :: forall nm. KnownSymbol nm => Text
typeName = T.pack $
  case lowerFirst $ symbolVal $ Proxy @nm of
    "[]" -> "builtinlist"
    "(,)" -> "builtinpair"
    ":*:" -> "builtinpair"
    t -> t

conName :: forall nm. KnownSymbol nm => Text
conName = T.pack $
  case symbolVal $ Proxy @nm of
    "[]" -> "BuiltinNil"
    ":" -> "BuiltinCons"
    "(,)" -> "BuiltinPair"
    ":*:" -> "BuiltinPair"
    t -> t

type GToBurstCons :: (K.Type -> K.Type) -> Constraint
class GToBurstCons f where
  gtoBurstCons :: [(Text, Type)]

instance (KnownSymbol nm, GToBurstType f) => GToBurstCons (C1 ('MetaCons nm _1 _2) f) where
  gtoBurstCons = pure (conName @nm, gtoBurstType @f)

instance (GToBurstCons f, GToBurstCons g) => GToBurstCons (f :+: g) where
  gtoBurstCons = gtoBurstCons @f ++ gtoBurstCons @g

type GToBurstType :: (K.Type -> K.Type) -> Constraint
class GToBurstType f where
  gtoBurstType :: Type

instance BurstRef a => GToBurstType (K1 _1 a) where
  gtoBurstType = TyVar $ burstRef @a

class BurstRef a where
  burstRef :: Text
  default burstRef :: GBurstRef (Rep a) => Text
  burstRef = gburstRef @(Rep a)

type GBurstRef :: (K.Type -> K.Type) -> Constraint
class GBurstRef f where
  gburstRef :: Text

instance KnownSymbol nm => GBurstRef (M1 D ('MetaData nm _1 _2 _3) _5) where
  gburstRef = typeName @nm

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (a : as) = toLower a : as

instance Marshall a => GMarshall (K1 _1 a) where
  gmarshall (K1 a) = marshall a

instance (GToBurstType f, GToBurstType g) => GToBurstType (f :*: g) where
  gtoBurstType = TyTuple (gtoBurstType @f) (gtoBurstType @g)

instance (GMarshall f, GMarshall g) => GMarshall (f :*: g) where
  gmarshall (a :*: b) = Tuple (gmarshall a) (gmarshall b)


instance (GToBurstType f) => GToBurstType (M1 _1 _2 f) where
  gtoBurstType = gtoBurstType @f

instance (GMarshall f) => GMarshall (M1 _1 _2 f) where
  gmarshall (M1 a) = gmarshall a

instance {-# OVERLAPPING #-} (KnownSymbol nm, GMarshall f) => GMarshall (M1 C ('MetaCons nm _1 _2) f) where
  gmarshall (M1 a) = Apply (Var $ conName @nm) $ gmarshall a

instance {-# OVERLAPPING #-} (KnownSymbol nm) => GMarshall (M1 C ('MetaCons nm _1 _2) U1) where
  gmarshall (M1 _) = Var $ conName @nm

instance (GMarshall f, GMarshall g) => GMarshall (f :+: g) where
  gmarshall (L1 a) = gmarshall a
  gmarshall (R1 a) = gmarshall a

instance GToBurstType U1 where
  gtoBurstType = TyUnit

instance GMarshall U1 where
  gmarshall _ = Unit


