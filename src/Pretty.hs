{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pretty where

import Prettyprinter
import Types
import Data.Text (Text)


instance Pretty Type where
  pretty (TyVar txt) = pretty txt
  pretty (TyTuple ty' ty2) = parens $ pretty ty' <> "," <+> pretty ty2
  pretty (TyArr ty' ty2) = parens $ pretty ty' <+> "->" <+> pretty ty2

instance Pretty Expr where
  pretty (Lambda x0 ex') = parens $ sep
    [ "\\" <+> sep (fmap prettyPat x0) <+> "->"
    , indent 2 (pretty ex')
    ]
  pretty (Proj1 ex') = parens $ "fst" <+> pretty ex'
  pretty (Proj2 ex') = parens $ "snd" <+> pretty ex'
  pretty (Tuple ex' ex2) = parens $ pretty ex' <> "," <+> pretty ex2
  pretty (Apply ex' ex2) = parens $ pretty ex' <+> pretty ex2
  pretty (Var txt) = pretty txt
  pretty (Match ex' x0) = parens $ sep
    [ "case" <+> pretty ex' <+> "of"
    , indent 2 $ braces (align $ vsep $ punctuate ";" $ fmap prettyMatch x0)
    ]

instance Pretty Decl where
  pretty (Fix txt ty ex) = vcat
    [ pretty txt <+> "::" <+> pretty ty
    , pretty txt <+> "=" <+> pretty ex
    ]

prettyMatch :: (Text, Expr) -> Doc ann
prettyMatch (txt, ex) = sep
  [ pretty txt <+> "{}" <+> "->"
  , indent 2 (pretty ex)
  ]

prettyPat :: (Text, Type) -> Doc ann
prettyPat = pretty . fst


