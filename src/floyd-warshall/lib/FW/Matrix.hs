{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Square matrix with statically checked size
-}
module FW.Matrix(
  Matrix(..),
  mapPos,
  SomeMatrix(..),
  mapMatrix,
  withMatrix,
  (*!!),
  (!*),
  mkMatrix,
  mkMatrix',
  fromList,
  fromLists,
  toList,
  toLists,
  transpose,
  at,
  invert,
  tryInvert,
  matrixDims,
  outer,
  -- * Testing
  paths
  ) where

import Data.Matrix qualified as M
import Data.Proxy (Proxy (..))
import Data.Semiring
import FW.Vector (Vector)
import FW.Vector qualified
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Linear.Vector qualified as V

infixl 7 *!!
infixl 7 !*

{-| Square matrix with statically known dimensions
-}
newtype Matrix (d :: Nat) a = Matrix{ unMatrix :: M.Matrix a }
  deriving stock (Functor, Foldable, Traversable, Eq, Generic)
  deriving newtype (Show, Applicative)

{-| Map the elements with their index
-}
mapPos :: ((Int, Int) -> a -> b) -> Matrix d a -> Matrix d b
mapPos f (Matrix m) = Matrix (M.mapPos f m)

{-| 'Matrix' with existentially qualified dimensions
-}
data SomeMatrix a where
  SomeMatrix :: forall (d :: Nat) a. KnownNat d => Matrix d a -> SomeMatrix a

mapMatrix :: forall a b. (forall (d :: Nat). KnownNat d => Matrix d a -> Matrix d b) -> SomeMatrix a -> SomeMatrix b
mapMatrix f (SomeMatrix m) = SomeMatrix (f m)

withMatrix :: forall a r. (forall (d :: Nat). KnownNat d => Matrix d a -> r) -> SomeMatrix a -> r
withMatrix f (SomeMatrix m) = f m

{-| Scalar times matrix
-}
(*!!) :: Semiring a => a -> Matrix d a -> Matrix d a
(*!!) s m = fmap (s .*.) m

{-| Matrix times column vector
-}
(!*) :: forall d a. (KnownNat d, Semiring a) => Matrix d a -> Vector d a -> Vector d a
(!*) m v = FW.Vector.mkVector $ \row -> srsum $ fmap (\col -> m `at` (row, col) .*. v `FW.Vector.at` col) $ entireRange (Proxy @d)

{-| Construct a 'Matrix' from a function
-}
mkMatrix :: forall d a. KnownNat d => ((Int, Int) -> a) -> Matrix d a
mkMatrix = mkMatrix' (Proxy @d)

{-| Construct a 'Matrix' from a function with a Proxy value
-}
mkMatrix' :: forall d a. KnownNat d => Proxy d -> ((Int, Int) -> a) -> Matrix d a
mkMatrix' p =
  let n = matrixDims p
  in Matrix . M.matrix n n

{-| Construct a 'Matrix' from a list of values (row major)
-}
fromList :: forall d a. KnownNat d => [a] -> Matrix d a
fromList =
  let n = matrixDims (Proxy @d)
  in Matrix . M.fromList n n

toList :: Matrix d a -> [a]
toList = M.toList . unMatrix

{-| Transpose a matrix
-}
transpose :: Matrix d a -> Matrix d a
transpose (Matrix m) = Matrix (M.transpose m)

{-| Convert a 'Matrix' to a list of rows
-}
toLists :: Matrix d a -> [[a]]
toLists = M.toLists . unMatrix

{-| Construct a 'Matrix' from a list of rows
-}
fromLists :: [[a]] -> Matrix d a
fromLists = Matrix . M.fromLists

{-| The value at an index
-}
at :: Matrix d a -> (Int, Int) -> a
at (Matrix m) (row, col) = M.getElem row col m

instance (KnownNat n) => V.Additive (Matrix n) where
  zero = mkMatrix @n (const 0)

instance (KnownNat d, Additive a) => Additive (Matrix d a) where
  zero = mkMatrix @d (const zero)
  (.+.) l r = mkMatrix $ \idx -> l `at` idx .+. r `at` idx

instance (KnownNat d, Semiring a) => Semiring (Matrix d a) where
  one = mkMatrix @d (\(i, j) -> if i == j then one else zero)
  l .*. r =
    let build (i, j) = srsum [l `at` (i, k) .*. r `at` (k, j) | k <- entireRange (Proxy @d)]
    in mkMatrix @d build

instance (KnownNat d, StarSemiring a) => StarSemiring (Matrix d a) where
  plus x = foldr f x (entireRange (Proxy @d)) where
    f k m = mkMatrix @d build where
      build (i, j) =
        (m `at` (i, j)) .+.
        (m `at` (i, k) .*. (star (m `at` (k, k))) .*. m `at` (k, j))

invert :: (KnownNat d, Eq a, Fractional a) => Matrix d (Compact a) -> Matrix d (Compact a)
invert m = star (one .+. fmap (fmap negate) m)

tryInvert :: (KnownNat d, Eq a, Fractional a) => Matrix d a -> Maybe (Matrix d a)
tryInvert =
  let fromC Inf      = Nothing
      fromC (Real k) = Just k
  in traverse fromC . invert . fmap Real

matrixDims :: KnownNat n => Proxy n -> Int
matrixDims = fromInteger . natVal

entireRange :: KnownNat n => Proxy n -> [Int]
entireRange p = [1..matrixDims p]

{-| Outer product of two equal-sized vectors
-}
outer :: (Semiring a, KnownNat n) => Vector n a -> Vector n a -> Matrix n a
outer l r =
  mkMatrix (\(a, b) -> l `FW.Vector.at` a .*. r `FW.Vector.at` b)

{-| Example from r6.ca/blog/20110808T035622Z.html

-}
paths :: Matrix 6 (RT Double)
paths = fromList $ fmap (maybe inf rt)
  [ Nothing, Just 7,  Just 9,  Nothing, Nothing, Just 14
  , Just 7,  Nothing, Just 10, Just 15, Nothing, Nothing
  , Just 9, Just 10,  Nothing, Just 11, Nothing, Just 2
  , Nothing, Just 15, Just 11, Nothing, Just 6, Nothing
  , Nothing, Nothing, Nothing, Just 6, Nothing, Just 9
  , Just 14, Nothing, Just 2, Nothing, Just 9, Nothing
  ]
