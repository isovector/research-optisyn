{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transform where

import Types
import Generics.SYB.Aliases
import Data.Text (Text)
import Data.Generics (everywhere)


natPat :: Pat -> Pat
natPat = \case
  RecordCtor "(n+1)" -> Raw "(n+1)"
  RecordCtor "0" -> Raw "0"
  RecordCtor "S" -> Raw "n+1"
  RecordCtor "O" -> Raw "0"
  o -> o

vars :: Text -> Text
vars = \case
  "list" -> "[a]"
  "nat" -> "Int"
  "Nil" -> "[]"
  "Cons" -> "(:)"
  "O" -> "0"
  "S" -> "(n+1)"
  "Un_Cons" -> "(\\(a : as) -> (a, as))"
  "Un_S" -> "(subtract 1)"
  (t :: Text) -> t



fixTerm :: GenericT
fixTerm = everywhere $ mkT natPat `extT` vars




