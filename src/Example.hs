{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Example where

import Types
import GenType
import Data.List (nub)
import Data.Functor ((<&>))

mkExample :: forall a b. (Marshall a, Marshall b) => (a -> b) -> [a] -> BurstDoc
mkExample fab as = BurstDoc
  { bd_context = nub [toBurstDecl @a, toBurstDecl @b]
  , bd_goal = TyArr (TyVar $ burstRef @a) (TyVar $ burstRef @b)
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

