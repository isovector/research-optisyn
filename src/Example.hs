{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts #-}

module Example where

import Types
import GenType
import Data.List (nub)
import Data.Functor ((<&>))
import ShowType (CanShowType)

mkExample :: forall a b. (CanShowType a, CanShowType b, Marshall a, Marshall b) => (a -> b) -> [a] -> BurstDoc
mkExample fab as = BurstDoc
  { bd_context = nub [toBurstDecl @a, toBurstDecl @b]
  , bd_goal = TyArr (TyVar $ burstTyName @a) (TyVar $ burstTyName @b)
  , bd_with = []
  , bd_satisfying
      = Right $ as <&> \a -> mkIO a $ fab a

--       $ Lambda [("in0", TyVar $ burstRef @a)]
--       $ Lambda [("out", TyVar $ burstRef @b)]
--       $ foldr (Apply . Apply "and") (marshall False)
--       $ flip fmap as
--       $ \a -> mkIO a $ fab a

  }


mkIO :: (Marshall a, Marshall b) => a -> b -> Example
mkIO a b = Example (marshall a) (marshall b)

