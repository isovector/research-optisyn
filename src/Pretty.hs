{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pretty where

import Prettyprinter
import Types
import Data.Text (Text)


class ToHaskell a where
  toHaskell :: a -> Doc ann

class ToBurst a where
  toBurst :: a -> Doc ann

instance ToHaskell Text where
  toHaskell = pretty

instance ToBurst Text where
  toBurst = pretty

instance ToHaskell Type where
  toHaskell (TyVar txt) = toHaskell txt
  toHaskell (TyTuple ty' ty2) = parens $ toHaskell ty' <> "," <+> toHaskell ty2
  toHaskell (TyArr ty' ty2) = parens $ toHaskell ty' <+> "->" <+> toHaskell ty2
  toHaskell TyUnit = parens ""

instance ToBurst Type where
  toBurst (TyVar txt) = toBurst txt
  toBurst (TyTuple ty' ty2) = parens $ toBurst ty' <+> "*" <+> toBurst ty2
  toBurst (TyArr ty' ty2) = parens $ toBurst ty' <+> "->" <+> toBurst ty2
  toBurst TyUnit = parens ""

instance ToHaskell Expr where
  toHaskell (Lambda x0 ex') = parens $ sep
    [ "\\" <+> sep (fmap hsPat x0) <+> "->"
    , indent 2 (toHaskell ex')
    ]
  toHaskell (Proj1 ex') = parens $ "fst" <+> toHaskell ex'
  toHaskell (Proj2 ex') = parens $ "snd" <+> toHaskell ex'
  toHaskell (Tuple ex' ex2) = parens $ toHaskell ex' <> "," <+> toHaskell ex2
  toHaskell (Apply ex' ex2) = parens $ toHaskell ex' <+> toHaskell ex2
  toHaskell (Var txt) = toHaskell txt
  toHaskell (Equal lhs rhs) = parens $ toHaskell lhs <+> "==" <+> toHaskell rhs
  toHaskell (Match ex' x0) = parens $ sep
    [ "case" <+> toHaskell ex' <+> "of"
    , indent 2 $ braces (align $ vsep $ punctuate ";" $ fmap hsMatch x0)
    ]
  toHaskell Unit = parens ""

instance ToBurst Expr where
  toBurst (Lambda x0 ex') = parens $ sep
    [ "fun" <+> sep (fmap burstPat x0) <+> "->"
    , indent 2 (toBurst ex')
    ]
  toBurst (Proj1 ex') = parens $ "fst" <+> toBurst ex'
  toBurst (Proj2 ex') = parens $ "snd" <+> toBurst ex'
  toBurst (Tuple ex' ex2) = parens $ toBurst ex' <> "," <+> toBurst ex2
  toBurst (Apply ex' ex2) = parens $ toBurst ex' <+> toBurst ex2
  toBurst (Var txt) = toBurst txt
  toBurst (Equal lhs rhs) = parens $ toBurst lhs <+> "==" <+> toBurst rhs
  toBurst (Match ex' x0) = parens $ sep
    [ "match" <+> toBurst ex' <+> "with"
    , indent 2 $ align $ vsep $ fmap ("|" <+>) $ fmap burstMatch x0
    ]
  toBurst Unit = parens ""

instance ToHaskell Decl where
  toHaskell (Fix txt ty ex) =
    "let" <+> align (
      vcat
        [ toHaskell txt <+> "::" <+> toHaskell ty
        , toHaskell txt <+> "=" <+> toHaskell ex
        ]
                       )

instance ToHaskell Pat where
  toHaskell (RecordCtor txt) = toHaskell txt <+> "{}"
  toHaskell (Raw txt) = toHaskell txt

instance ToBurst Pat where
  toBurst (RecordCtor txt) = toBurst txt <+> "_"
  toBurst (Raw txt) = toBurst txt

instance ToBurst TyDecl where
  toBurst (TyDecl txt x0) = vcat
    [ "type" <+> pretty txt <+> "="
    , indent 2 $ vcat $ fmap burstCon x0
    ]

instance ToBurst BurstDoc where
  toBurst BurstDoc {bd_context, bd_goal, bd_with, bd_satisfying = ex} =
    vcat
      [ vcat $ fmap toBurst bd_context
      , "synth" <+> toBurst bd_goal <+> "satisfying"
      , case ex of
          Left ex' -> toBurst ex'
          Right exs -> vcat $ punctuate "," $ fmap toBurst exs
      ]

instance ToBurst Example where
  toBurst (Example ex' ex2) = brackets (toBurst ex') <+> "->" <+> toBurst ex2

burstCon :: (Text, Type) -> Doc ann
burstCon (txt, TyUnit) = "|" <+> toBurst txt
burstCon (txt, ty) = "|" <+> toBurst txt <+> "of" <+> toHaskell ty


hsMatch :: (Pat, Expr) -> Doc ann
hsMatch (pat, ex) = sep
  [ toHaskell pat <+> "->"
  , indent 2 (toHaskell ex)
  ]

burstMatch :: (Pat, Expr) -> Doc ann
burstMatch (pat, ex) = sep
  [ toBurst pat <+> "->"
  , indent 2 (toBurst ex)
  ]

hsPat :: (Text, Type) -> Doc ann
hsPat = toHaskell . fst


burstPat :: (Text, Type) -> Doc ann
burstPat (txt, ty) = parens $ toBurst txt <+> ":" <+> toBurst ty

