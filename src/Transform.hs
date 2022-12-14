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


patMap :: [TyDecl] -> Map Text Text
patMap = foldMap $ \td ->
  mappend (M.singleton (td_bust td) (td_haskell td)) $
    flip foldMap (td_cons td) $ \dc ->
      M.fromList
        [ (dc_burst dc, dc_haskell dc)
        ]

unmangleMap :: [TyDecl] -> Map Text Expr
unmangleMap = foldMap $ \td -> flip foldMap (td_cons td) $ \dc ->
  M.fromList
    [ (dc_burst dc, dc_build dc)
    , ("Un_" <> dc_burst dc, dc_unbuild dc)
    ]


tys :: Map Text Text -> Type -> Type
tys m = \case
  TyVar x
    | Just x' <- M.lookup x m
    -> TyVar x'
  t -> t

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

vars :: Map Text Expr -> Expr -> Expr
vars m = \case
  Var x
    | Just x' <- M.lookup x m
    -> x'
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
fixTerm tds = do
  let um = unmangleMap tds
      pm = patMap tds

  everywhere $ mkT (natPat pm) `extT` vars um `extT` tys pm -- `extT` upperTy `extT` varsE

