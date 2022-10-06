module Types where
import Data.Text (Text)

data Decl
  = Fix Text Type Expr
  deriving (Eq, Ord, Show)

data Type
  = TyVar Text
  | TyTuple Type Type
  | TyArr Type Type
  deriving (Eq, Ord, Show)

data Expr
  = Lambda [(Text, Type)] Expr
  | Proj1 Expr
  | Proj2 Expr
  | Tuple Expr Expr
  | Apply Expr Expr
  | Var Text
  | Match Expr [(Text, Expr)]
  deriving (Eq, Ord, Show)

