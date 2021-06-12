module Janus.Units.EpochSecond
  ( EpochSecond,
    mkEpochSecond,
    toInt,
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Prelude

newtype EpochSecond = EpochSecond Int64
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

mkEpochSecond :: (Integral a) => a -> EpochSecond
mkEpochSecond second = EpochSecond $ fromIntegral second

toInt :: (Integral a) => EpochSecond -> a
toInt (EpochSecond a) = fromIntegral a