module Janus.Units.Second
  ( Second,
    mkSecond,
    unsafeMkSecond,
    toInt,
  )
where

import Data.Ix (Ix)
import Prelude
import Data.Maybe (fromMaybe)

newtype Second = Second Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkSecond :: (Integral a) => a -> Maybe Second
mkSecond second = if 0 <= second && second <= 59 then Just (Second $ fromIntegral second) else Nothing

unsafeMkSecond :: (Integral a) => a -> Second
unsafeMkSecond nano = fromMaybe (error "") $ mkSecond nano

toInt :: (Integral a) => Second -> a
toInt (Second a) = fromIntegral a