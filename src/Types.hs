{-# LANGUAGE DeriveDataTypeable #-}
module Types where
import Data.Text (Text)
import Data.Data (Data)
import Data.Dynamic (Typeable)
import Data.String
import qualified Data.Text as T

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
  | Equal Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Expr where
  fromString = Var . T.pack

data Example
  = Example Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)


data DataCon = DataCon
  { dc_burst   :: Text
  , dc_haskell :: Text
  , dc_type    :: Type
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data TyDecl = TyDecl Text [DataCon]
  deriving (Eq, Ord, Show, Data, Typeable)

data Pat
  = RecordCtor Text
  | Raw Text
  deriving (Eq, Ord, Show, Data, Typeable)


data BurstDoc = BurstDoc
  { bd_context :: [TyDecl]
  , bd_goal :: Type
  , bd_with :: [Either TyDecl Decl]
  , bd_satisfying :: Either Expr [Example]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

