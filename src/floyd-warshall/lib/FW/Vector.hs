{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Vectors with statically checked sizes
-}
module FW.Vector(
  Vector(..),
  mkVector,
  fromList,
  at
  ) where

import Data.Matrix qualified as M
import Data.Proxy (Proxy (..))
import Data.Semiring (Additive (..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Linear.Vector qualified as V

newtype Vector (n :: Nat) a = Vector{ unVector :: M.Matrix a }
  deriving stock (Functor, Foldable, Traversable, Eq, Generic)
  deriving newtype (Show, Applicative)

instance (KnownNat n, Additive a) => Additive (Vector n a) where
  zero = mkVector @n (const zero)
  (.+.) l r = mkVector $ \idx -> l `at` idx .+. r `at` idx

instance (KnownNat n) => V.Additive (Vector n) where
  zero = mkVector @n (const 0)

{-| Matrix dimension of a (column) vector
-}
mkDims :: KnownNat n => Proxy n -> (Int, Int)
mkDims p =
  let n = fromInteger (natVal p)
  in (n, 1)

mkVector :: forall n a. KnownNat n => (Int -> a) -> Vector n a
mkVector f =
  let (rows, cols) = mkDims (Proxy @n)
  in Vector $ M.matrix rows cols (f . fst)

fromList :: forall n a. KnownNat n => [a] -> Vector n a
fromList =
  let (rows, cols) = mkDims (Proxy @n)
  in Vector . M.fromList rows cols

{-| The value at an index
-}
at :: Vector d a -> Int -> a
at (Vector m) row = M.getElem row 1 m

