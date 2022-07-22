module Janus.Units.EpochDay
  ( EpochDay,
    mkEpochDay,
    toInt,
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Prelude

newtype EpochDay = EpochDay Int64
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord,
      Num,
      Integral,
      Real
    )

mkEpochDay :: (Integral a) => a -> EpochDay
mkEpochDay second = EpochDay $ fromIntegral second

toInt :: (Integral a) => EpochDay -> a
toInt (EpochDay a) = fromIntegral a