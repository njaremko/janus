module Janus.Temporal where

import Data.Int (Int64)
import Janus.Units (Unit)
import Prelude

class TemporalField a where
  getBaseUnit :: a -> Unit
  getRangeUnit :: a -> Unit
  range :: a -> (Int64, Int64)

class TemporalAccessor a where
  getLong :: a -> Int64

class TemporalAdjuster a b where
  adjustInto :: a -> b

class (TemporalAccessor a, TemporalAccessor b) => Temporal a b where
  plus :: a -> b -> a
  minus :: a -> b -> a
  until :: a -> b -> Int64
