{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}

module Iso where

import GHC.Generics
import Test.QuickCheck (Arbitrary, generate, vectorOf, arbitrary)
import GenType (Marshall)
import RunBurst (runBurst)
import Data.Typeable (Typeable)
import Data.Maybe (isJust)


class InjList f a where
  size :: Int
  intoList :: f x -> [a]
  fromList :: [a] -> f x

instance InjList U1 a where
  size = 0
  intoList _ = []
  fromList _ = U1

instance InjList (K1 _1 a) a where
  size = 1
  intoList (K1 a) = [a]
  fromList [a] = K1 a

instance (InjList f a, InjList g a) => InjList (f :*: g) a where
  size = size @f @a + size @g @a
  intoList (a :*: b) = intoList a <> intoList b
  fromList as =
    let (x, y) = splitAt (size @f @a) as
     in fromList @f x :*: fromList @g y



findFunctor
    :: forall f a b r
     . (InjList f a, Arbitrary a, Marshall (f a), Marshall b, Marshall r, Typeable (f a), Typeable b, Typeable r)
    => ((b, [a]) -> r)
    -> b
    -> IO (Maybe ((b, f a) -> r))
findFunctor f b = do
  points <- generate $ vectorOf 10 $ vectorOf (size @f @a) $ arbitrary @a
  pure $ runBurst (f . fmap intoList) $ fmap ((b, ) . fromList @f @a) points


main :: IO ()
main = do
  print . isJust =<< findFunctor @U1 (reverse @Bool . snd @()) ()
  -- print . isJust =<< findFunctor @(Rec0 Bool) (reverse @Bool . snd @()) ()
  -- print . isJust =<< findFunctor @(Rec0 Bool :*: Rec0 Bool) (reverse @Bool . snd @()) ()

