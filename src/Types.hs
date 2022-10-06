{-# LANGUAGE DeriveDataTypeable #-}
module Types where
import Data.Text (Text)
import Data.Data (Data)
import Data.Dynamic (Typeable)

data Decl
  = Fix Text Type Expr
  deriving (Eq, Ord, Show, Data, Typeable)

data Type
  = TyVar Text
  | TyTuple Type Type
  | TyArr Type Type
  | TyUnit
  deriving (Eq, Ord, Show, Data, Typeable)

data Expr
  = Lambda [(Text, Type)] Expr
  | Proj1 Expr
  | Proj2 Expr
  | Tuple Expr Expr
  | Apply Expr Expr
  | Var Text
  | Match Expr [(Pat, Expr)]
  | Unit
  deriving (Eq, Ord, Show, Data, Typeable)

data TyDecl = TyDecl Text [(Text, Type)]
  deriving (Eq, Ord, Show, Data, Typeable)

data Pat
  = RecordCtor Text
  | Raw Text
  deriving (Eq, Ord, Show, Data, Typeable)

