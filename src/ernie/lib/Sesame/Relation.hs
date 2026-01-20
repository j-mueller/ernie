{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Sesame.Relation(
  Constraint(..)
) where

import Data.Time.Clock (UTCTime)
import Ernie.Time (Days)
import Sesame.Var (Var)

data Constraint =
  CLineq { lhs :: [(Days, Var Days)], rhs :: Days }
  | CDiff { dt1 :: Var UTCTime, dt2 :: Var UTCTime, rhs :: Days }
  deriving stock (Eq, Show)
