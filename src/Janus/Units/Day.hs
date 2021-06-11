module Janus.Units.Day (Day, mkDay, unsafeMkDay, toInt) where

import Prelude
import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

newtype Day = Day Word8
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkDay :: (Integral a) => a -> Maybe Day
mkDay day = if 1 <= day && day <= 31 then Just (Day $ fromIntegral day) else Nothing

unsafeMkDay :: (Integral a) => a -> Day
unsafeMkDay day = fromMaybe (error "") $ mkDay day

toInt :: (Integral a) => Day -> a
toInt (Day a) = fromIntegral a