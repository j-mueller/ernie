{-# LANGUAGE TupleSections #-}
{-| Utilities
-}
module Ernie.Utils(
  traverseT
  ) where

{-| Apply a function and return the original value alongside
the result
-}
traverseT :: Functor f => (a -> f b) -> a -> f (a, b)
traverseT f a = (a,) <$> f a
