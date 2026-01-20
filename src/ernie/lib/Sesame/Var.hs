module Sesame.Var(
  Var(..),

  -- * Builder type
  VarBuilderT(..),
  runVarBuilderT,
  Fresh(..)
) where

import Control.Monad.State.Strict (StateT, get, put, runStateT)

-- | Variable with phantom type to denote domain
newtype Var t = Var Int
  deriving stock (Eq, Ord, Show)

newtype VarBuilderT m a = VarBuilderT (StateT Int m a)
  deriving newtype (Functor, Applicative, Monad)

runVarBuilderT :: VarBuilderT m a -> m (a, Int)
runVarBuilderT (VarBuilderT action) = runStateT action 0

class Monad m => Fresh m where
  var :: m (Var t)

instance Monad m => Fresh (VarBuilderT m) where
  var = VarBuilderT $ do
          old <- get
          let v = Var old
          put (succ old)
          pure v
