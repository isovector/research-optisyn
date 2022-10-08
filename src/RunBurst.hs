{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module RunBurst where

import System.Process
import Example
import GenType
import Pretty (toBurst, toHaskell)
import Text.Megaparsec
import qualified Data.Text as T
import Transform
import Parser
import Eval (eval)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)
import Types (bd_context)
import GHC.Generics (U1, K1, Rec0)
import Data.Maybe (isJust)
import ShowType (CanShowType)

runBurst
    :: forall a b
     . (Typeable a, Typeable b, CanShowType a, CanShowType b, Marshall a, Marshall b)
    => (a -> b)
    -> [a]
    -> Maybe (a -> b)
runBurst f as = unsafePerformIO $ do
  let doc' = mkExample f as
      doc = doc' { bd_context = toBurstDecl @Bool
                              -- : toBurstDecl @()
                              : bd_context doc' }

  let fp = "/tmp/burst"
  writeFile fp $ show $ toBurst doc
  res <- fmap (unlines . takeWhile (/= ";") . lines) $ readProcess "burst" [fp] ""
  putStrLn res
  case fmap fixTerm $ parse decl "interactive" $ T.pack res of
    Left peb -> do
      putStrLn $ errorBundlePretty peb
      pure Nothing
    Right term -> do
      putStrLn $ show $ toHaskell term
      eval @(a -> b) term >>= \case
        Left ie -> do
          putStrLn $ showGHC ie
          pure Nothing
        Right fab -> pure $ Just fab
{-# NOINLINE runBurst #-}


yo :: IO ()
yo = do
  print $ isJust $ runBurst (reverse @Bool) [[], [True, False, False, True, True], [False, True], [True]]

