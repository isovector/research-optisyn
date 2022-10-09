{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module RunBurst where

import           Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Eval (eval)
import           Example
import           GHC.Generics (Rep)
import           GenType
import           Parser
import           Pretty (toBurst, toHaskell)
import           ShowType (CanShowType)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process
import           Text.Megaparsec
import           Transform
import           Types (bd_context)

runBurst
    :: forall a b
     . ( Typeable a
       , Typeable b
       , CanShowType a
       , CanShowType b
       , Marshall a
       , Marshall b
       , GNecessaryTypes (Rep a)
       , GNecessaryTypes (Rep b)
       )
    => (a -> b)
    -> [a]
    -> Maybe (a -> b)
runBurst f as = unsafePerformIO $ do
  let doc' = mkExample f as
      doc = doc'
          { bd_context = S.toList $ mconcat
              [ necessaryTypes @a
              , necessaryTypes @b
              , S.fromList $ bd_context doc'
              ]
          }

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

