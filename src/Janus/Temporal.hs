module Janus.Temporal where

import Prelude
import Data.Int (Int64)

class TemporalAccessor a where
    getLong :: a -> Int64

class (TemporalAccessor a, TemporalAccessor b) => Temporal a b where
    plus :: a -> b -> a
    minus :: a -> b -> a
    until :: a -> b -> Int64
