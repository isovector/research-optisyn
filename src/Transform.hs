{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transform where

import Types
import Generics.SYB.Aliases
import Data.Text (Text)
import Data.Generics (everywhere)
import qualified Data.Text as T
import Data.Char (toUpper)


natPat :: Pat -> Pat
natPat = \case
  RecordCtor "(n+1)" -> Raw "(n+1)"
  RecordCtor "0" -> Raw "0"
  RecordCtor "S" -> Raw "n+1"
  RecordCtor "O" -> Raw "0"
  o -> o

varsE :: Expr -> Expr
varsE = \case
  Var "(:)" -> Var "uncurry (:)"
  t -> t

upperTy :: Type -> Type
upperTy = \case
  TyVar txt -> TyVar $ T.pack $ firstUpper $ T.unpack txt
  t -> t

firstUpper :: String -> String
firstUpper [] = []
firstUpper (c : str) = toUpper c : str

vars :: Text -> Text
vars = \case
  "builtinlist" -> "[a]"
  "builtinpair" -> "(,)"
  "nat" -> "Int"
  "BuiltinNil" -> "[]"
  "BuiltinCons" -> "(:)"
  "BuiltinPair" -> "(,)"
  "O" -> "0"
  "S" -> "(n+1)"
  "Un_BuiltinCons" -> "(\\(a : as) -> (a, as))"
  "Un_S" -> "(subtract 1)"
  (t :: Text) -> t



fixTerm :: GenericT
fixTerm = everywhere $ mkT natPat `extT` vars `extT` upperTy `extT` varsE

