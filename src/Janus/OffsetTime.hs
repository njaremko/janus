module Janus.OffsetTime where

import Prelude
import Janus.LocalTime (LocalTime)
import Janus.ZoneOffset (ZoneOffset)
import Data.Ix (Ix)

-- A time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 10:15:30+01:00.
data OffsetTime = OffsetTime {
    time :: LocalTime,
    offset :: ZoneOffset
} deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )