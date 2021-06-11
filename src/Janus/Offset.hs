module Janus.Offset (Offset, ofHours) where

import Data.Ix (Ix)
import Data.Text (Text)
import Janus.Units
import Prelude

-- A time-zone offset from Greenwich/UTC, such as +02:00.
newtype Offset = Offset Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

ofHours :: Int -> Either Text Offset
ofHours hours = ofHoursMinutesSeconds hours 0 0

ofHoursMinutesSeconds :: Int -> Int -> Int -> Either Text Offset
ofHoursMinutesSeconds hours minutes seconds =
  let totalSeconds =
        (fromIntegral hours * minutesPerHour * secondsPerMinute)
          + (fromIntegral minutes * secondsPerMinute)
          + fromIntegral seconds
   in ofTotalSeconds totalSeconds

ofTotalSeconds :: (Integral a) => a -> Either Text Offset
ofTotalSeconds totalSeconds =
  if -64800 <= totalSeconds && totalSeconds <= 64800
    then Right . Offset $ fromIntegral totalSeconds
    else Left "Zone offset not in valid range: -18:00 to +18:00"