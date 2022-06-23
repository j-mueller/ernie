{-| Types for time (might use a library for this in the future)
-}
module Ernie.Time(
  Days(..)
  ) where

-- | Days. All estimates are in multiples of days.
newtype Days = Days {getDays :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional)
