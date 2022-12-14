{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Eval where

import Language.Haskell.Interpreter
import Types
import Data.Typeable (Typeable)
import Pretty (toHaskell)
import qualified Data.Text as T


eval :: forall a. Typeable a => Decl -> IO (Either InterpreterError a)
eval d@(Fix n _ _) =
  runInterpreter $ do
    set $ [ languageExtensions := [NPlusKPatterns, ImplicitPrelude] ]
    setImportsF [ModuleImport "Prelude" NotQualified NoImportList]
    runStmt $ show $ toHaskell d
    interpret (T.unpack n) $ as @a

