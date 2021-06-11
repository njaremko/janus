module Janus.Units.Hour
  ( Hour,
    mkHour,
    unsafeMkHour,
    toInt,
  )
where

import Data.Ix (Ix)
import Data.Word (Word8)
import Prelude
import Data.Maybe (fromMaybe)

newtype Hour = Hour Word8
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord,
      Real
    )

mkHour :: (Integral a) => a -> Maybe Hour
mkHour hour = if 0 <= hour && hour <= 23 then Just (Hour $ fromIntegral hour) else Nothing

unsafeMkHour :: (Integral a) => a -> Hour
unsafeMkHour hour = fromMaybe (error "") $ mkHour hour

toInt :: (Integral a) => Hour -> a
toInt (Hour a) = fromIntegral a