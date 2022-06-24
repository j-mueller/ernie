{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Proxy (Proxy (..))
import Data.Semiring (Additive (..), Compact (..), RT, Semiring (..),
                      StarSemiring (..), inf, rt)
import FW.Matrix (Matrix, fromList, invert)
import FW.Matrix qualified as M
import GHC.TypeLits (KnownNat)
import Linear.Matrix qualified as L
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Prelude
import Test.QuickCheck ((.&&.), (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ tropical
  , compact
  , matrix2tropical
  , matrix2compact
  , matrix3tropical
  , matrix3compact
  , matrix25compact
  , matrix25tropical
  , linear
  ]

tropical :: TestTree
tropical = testGroup "tropical"
  [ testProperty "additive" (additive_laws genTropical)
  , testProperty "semiring" (semiring_laws genTropical)
  , testProperty "star-semiring" (star_semiring_laws genTropical)
  ]

compact :: TestTree
compact = testGroup "compact"
  [ testProperty "additive" (additive_laws genCompact)
  , testProperty "semiring" (semiring_laws genCompact)
  , testProperty "star-semiring" (star_semiring_laws genCompact)
  ]

matrix2tropical :: TestTree
matrix2tropical = testGroup "2x2 matrix (tropical)"
  [ testProperty "additive" (additive_laws $ genMatrix @2 genTropical)
  , testProperty "semiring" (semiring_laws $ genMatrix @2 genTropical)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @2 genTropical)
  ]

matrix2compact :: TestTree
matrix2compact = testGroup "2x2 matrix (compact)"
  [ testProperty "additive" (additive_laws $ genMatrix @2 genCompact)
  , testProperty "semiring" (semiring_laws $ genMatrix @2 genCompact)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @2 genCompact)
  , testProperty "inverse" (inverse_laws $ genMatrix @2 genCompactNonInf)
  ]

matrix3tropical :: TestTree
matrix3tropical = testGroup "3x3 matrix (tropical)"
  [ testProperty "additive" (additive_laws $ genMatrix @3 genTropical)
  , testProperty "semiring" (semiring_laws $ genMatrix @3 genTropical)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @3 genTropical)
  ]

matrix3compact :: TestTree
matrix3compact = testGroup "3x3 matrix (compact)"
  [ testProperty "additive" (additive_laws $ genMatrix @3 genCompact)
  , testProperty "semiring" (semiring_laws $ genMatrix @3 genCompact)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @3 genCompact)
  , testProperty "inverse" (inverse_laws $ genMatrix @3 genCompactNonInf)
  ]

matrix25tropical :: TestTree
matrix25tropical = testGroup "10x10 matrix (tropical)"
  [ testProperty "additive" (additive_laws $ genMatrix @10 genTropical)
  , testProperty "semiring" (semiring_laws $ genMatrix @10 genTropical)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @10 genTropical)
  ]

matrix25compact :: TestTree
matrix25compact = testGroup "10x10 matrix (compact)"
  [ testProperty "additive" (additive_laws $ genMatrix @10 genCompact)
  , testProperty "semiring" (semiring_laws $ genMatrix @10 genCompact)
  , testProperty "star-semiring" (star_semiring_laws $ genMatrix @10 genCompact)
  , testProperty "inverse" (inverse_laws $ genMatrix @10 genCompactNonInf)
  ]

linear :: TestTree
linear = testGroup "linear"
  [ testGroup "3x3"
      [ testProperty "correctness of inversion" threeByThreeCorrectness
      , testProperty "conversion" threeByThreeConversion
      ]
  , testGroup "4x4"
      [ testProperty "correctness of inversion" fourByFourCorrectness
      , testProperty "conversion" fourByFourConversion
      ]
  ]

additive_laws :: (Show a, Additive a, Eq a) => QC.Gen a -> QC.Property
additive_laws gen = QC.forAll ((,) <$> gen <*> gen) $ \(x, y) -> do
    x .+. y === y .+. x
    .&&. (x .+. zero === x)
    .&&. (zero .+. x === x)

semiring_laws :: (Show a, Semiring a, Eq a) => QC.Gen a -> QC.Property
semiring_laws gen = QC.forAll ((,,) <$> gen <*> gen <*> gen) $ \(x, y, z) -> do
    (x .*. one === one .*. x)
    .&&. (x .*. (y .+. z) === (x .*. y) .+. (x .*. z))

star_semiring_laws :: (Show a, StarSemiring a, Eq a) => QC.Gen a -> QC.Property
star_semiring_laws gen = QC.forAll gen $ \x ->
  star x === one .+. (x .*. star x)
  .&&. (star x === one .+. (star x .*. x))

inverse_laws :: (Show a, Eq a, Fractional a, KnownNat d) => QC.Gen (Matrix d (Compact a)) -> QC.Property
inverse_laws gen = QC.forAll gen $ \m ->
  m === invert (invert m)
  .&&. (m .*. invert m == one)

threeByThreeCorrectness :: QC.Property
threeByThreeCorrectness = QC.forAll (genMatrix @3 genRational) $ \mtr ->
  let mtrA = fmap Real $ fromLinear3 $ L.inv33 (toLinear3 mtr)
      mtrB = invert (Real <$> mtr)
  in mtrA === mtrB

threeByThreeConversion :: QC.Property
threeByThreeConversion = QC.forAll (genMatrix @3 genRational) $ \mtr ->
  mtr === fromLinear3 (toLinear3 mtr)

fourByFourCorrectness :: QC.Property
fourByFourCorrectness = QC.forAll (genMatrix @4 genRational) $ \mtr ->
  let mtrA = fmap Real $ fromLinear4 $ L.inv44 (toLinear4 mtr)
      mtrB = invert (Real <$> mtr)
  in mtrA === mtrB

fourByFourConversion :: QC.Property
fourByFourConversion = QC.forAll (genMatrix @4 genRational) $ \mtr ->
  mtr === fromLinear4 (toLinear4 mtr)

genTropical :: QC.Gen (RT Rational)
genTropical = QC.frequency [(5, rt <$> genRationalNonNeg), (1, pure inf)]

genTropicalNonInf :: QC.Gen (RT Rational)
genTropicalNonInf = rt <$> genRationalNonNeg

genRationalNonNeg :: QC.Gen Rational
genRationalNonNeg = do
  x <- QC.choose (0, 10000)
  y <- QC.choose (1, 100000)
  pure (fromInteger x / fromInteger y)

genRational :: QC.Gen Rational
genRational = do
  x <- QC.choose (-10000, 10000)
  y <- QC.choose (1, 100000)
  pure (fromInteger x / fromInteger y)

genCompact :: QC.Gen (Compact Rational)
genCompact = QC.frequency [(5, genCompactNonInf), (1, pure Inf)]

genCompactNonInf :: QC.Gen (Compact Rational)
genCompactNonInf = Real <$> genRational

genMatrix :: forall d a. KnownNat d => QC.Gen a -> QC.Gen (Matrix d a)
genMatrix gen = do
  let p = Proxy @d
      d = M.matrixDims p
  lst <- QC.vectorOf (d * d) gen
  pure (fromList lst)

toLinear3 :: Matrix 3 a -> L.M33 a
toLinear3 m =
  let mkV [a, b, c] = V3 a b c
  in mkV (mkV <$> M.toLists (M.transpose m))

fromLinear3 :: L.M33 a -> Matrix 3 a
fromLinear3 v =
  let mkR (V3 a b c) = [a, b, c]
  in M.transpose $ M.fromLists $ fmap mkR $ mkR v

toLinear4 :: Matrix 4 a -> L.M44 a
toLinear4 m =
  let mkV [a, b, c, d] = V4 a b c d
  in mkV (mkV <$> M.toLists (M.transpose m))

fromLinear4 :: L.M44 a -> Matrix 4 a
fromLinear4 v =
  let mkR (V4 a b c d) = [a, b, c, d]
  in M.transpose $ M.fromLists $ fmap mkR $ mkR v
