{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Transform where

import qualified Data.Map as M
import Data.Map (Map)
import Types
import Generics.SYB.Aliases
import Data.Text (Text)
import Data.Generics (everywhere)
import qualified Data.Text as T
import Data.Char (toUpper)
import Control.Applicative (liftA2)
import Debug.Trace (traceShowId)


unmangleMap :: [TyDecl] -> Map Text Text
unmangleMap = foldMap $ \td -> flip foldMap (td_cons td) $
  liftA2 M.singleton dc_burst dc_haskell



natPat :: Map Text Text -> Pat -> Pat
natPat m = \case
  RecordCtor "(n+1)" -> Raw "(n+1)"
  RecordCtor "0" -> Raw "0"
  RecordCtor "S" -> Raw "n+1"
  RecordCtor s
    | Just s' <- M.lookup s m
    -> RecordCtor s'
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

vars :: Map Text Text -> Expr -> Expr
vars m = \case
  Var x
    | Just x' <- M.lookup x m
    -> Var x'
  t -> t
  -- "builtinlist" -> "[a]"
  -- "builtinpair" -> "(,)"
  -- "nat" -> "Int"
  -- "BuiltinNil" -> "[]"
  -- "BuiltinCons" -> "(:)"
  -- "BuiltinPair" -> "(,)"
  -- "O" -> "0"
  -- "S" -> "(n+1)"
  -- "Un_BuiltinCons" -> "(\\(a : as) -> (a, as))"
  -- "Un_S" -> "(subtract 1)"
  -- (t :: Text) -> t


fixTerm :: [TyDecl] -> GenericT
fixTerm (unmangleMap -> um)
  = everywhere
  $ mkT (natPat um) `extT` vars um -- `extT` upperTy `extT` varsE

