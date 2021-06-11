module Janus.Units.Minute
  ( Minute,
    mkMinute,
    unsafeMkMinute,
    toInt,
  )
where

import Data.Ix (Ix)
import Prelude
import Data.Maybe (fromMaybe)

newtype Minute = Minute Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkMinute :: (Integral a) => a -> Maybe Minute
mkMinute minute = if 0 <= minute && minute <= 59 then Just (Minute $ fromIntegral minute) else Nothing

unsafeMkMinute :: (Integral a) => a -> Minute
unsafeMkMinute minute = fromMaybe (error "") $ mkMinute minute

toInt :: (Integral a) => Minute -> a
toInt (Minute a) = fromIntegral a