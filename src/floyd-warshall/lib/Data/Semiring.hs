{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Data.Semiring(
  Additive(..),
  Semiring(..),
  WrappedNum(..),
  StarSemiring(..),
  Tropical(..),
  Real_(..),
  RT,
  rt,
  inf,
  Compact(..)
  ) where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity (..))

import Prelude as P

infixr 6 .+.
infixl 7 .*.

class Additive a where
  zero  :: a
  (.+.) :: a -> a -> a
  srsum :: [a] -> a
  srsum = foldr (.+.) zero

instance Additive b => Additive (a -> b) where
  zero = const zero
  (.+.) = liftA2 (.+.)

class Additive a => Semiring a where
  one :: a
  (.*.) :: a -> a -> a
  srprod :: [a] -> a
  srprod = foldr (.*.) one

class Semiring a => StarSemiring a where
  star :: a -> a
  star a = one .+. plus a
  plus :: a -> a
  plus a = a .*. star a

class Additive a => Group a where
  negate :: a -> a

newtype WrappedNum n = WrappedNum n
  deriving stock (Eq, Ord, Show, Functor, Foldable)
  deriving newtype (Num, Fractional)
  deriving Applicative via Identity

instance Num a => Additive (WrappedNum a) where
  zero = 0
  (.+.) = liftA2 (+)

instance Num a => Semiring (WrappedNum a) where
  one = 1
  (.*.) = liftA2 (*)

instance Num a => Group (WrappedNum a) where
  negate = fmap P.negate

data Tropical a = Tropical !a | Infinity
  deriving stock (Eq, Ord)

instance Show a => Show (Tropical a) where
  show (Tropical a) = show a
  show Infinity     = "inf."

newtype Real_ a = Real_ a
  deriving newtype (Show, Eq, Ord)

instance (Ord a, Num a) => Additive (Real_ (Tropical a)) where
  zero = Real_ Infinity
  (.+.) (Real_ l) (Real_ r) = Real_ $ case (l, r) of
      (Tropical a, Tropical b) -> Tropical (min a b)
      (Tropical a, _)          -> Tropical a
      (_, Tropical b)          -> Tropical b
      _                        -> Infinity

instance (Ord a, Num a) => Semiring (Real_ (Tropical a)) where
  one = Real_ (Tropical 0)
  (.*.) (Real_ l) (Real_ r) = Real_ $ case (l, r) of
    (Tropical a, Tropical b) -> Tropical (a + b)
    _                        -> Infinity

instance (Ord a, Fractional a) => StarSemiring (Real_ (Tropical a)) where
  star _ = one

type RT d = Real_ (Tropical d)

rt :: d -> RT d
rt = Real_ . Tropical

inf :: RT d
inf = Real_ Infinity

data Compact a = Real a
               | Inf
  deriving stock (Functor, Eq)

instance Applicative Compact where
  pure = Real
  liftA2 op (Real a) (Real b) = Real (a `op` b)
  liftA2 _  _ _               = Inf

instance (Group a, Eq a, Num a) => Num (Compact a) where
  (*) = (.*.)
  (+) = (.+.)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = Data.Semiring.negate

instance Show a => Show (Compact a) where
  show (Real n) = show n
  show Inf      = "inf."

instance (Eq a, Num a) => Additive (Compact a) where
  zero = Real 0
  Inf    .+. _      = Inf
  _      .+. Inf    = Inf
  Real x .+. Real y = Real (x + y)

instance (Eq a, Num a) => Semiring (Compact a) where
  one = Real 1
  Real 0 .*. _      = Real 0
  _      .*. Real 0 = Real 0
  Inf    .*. _      = Inf
  _      .*. Inf    = Inf
  Real x .*. Real y = Real (x * y)

instance (Num a, Eq a, Group a) => Group (Compact a) where
  negate = fmap Data.Semiring.negate

instance (Eq a, Fractional a) => StarSemiring (Compact a) where
  star (Real 1) = Inf
  star (Real x) = Real (recip (1 - x))
  star Inf      = Inf

instance (Group a, Fractional a, Eq a) => Fractional (Compact a) where
  recip = fmap recip -- ?
  fromRational = pure . fromRational
