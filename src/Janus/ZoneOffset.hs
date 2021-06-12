module Janus.ZoneOffset
  ( ZoneOffset,
    ofHours,
    ofHoursMinutesSeconds,
    ofTotalSeconds,
    toString,
    getTotalSeconds
  )
where

import Data.Bits ((.|.))
import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Janus.Units
import Prelude

-- A time-zone offset from Greenwich/UTC, such as +02:00.
newtype ZoneOffset = ZoneOffset Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

getTotalSeconds :: ZoneOffset -> Int
getTotalSeconds (ZoneOffset x) = x

-- >>> toString $ Offset 3600
-- "+01:00"
-- >>> toString $ Offset 3601
-- "+01:00:01"
-- >>> toString $ Offset (-3601)
-- "-01:00:01"
toString :: ZoneOffset -> Text
toString (ZoneOffset 0) = "Z"
toString (ZoneOffset totalSeconds) =
  let absTotalSeconds = abs totalSeconds
      absHours = absTotalSeconds `div` 3600
      absMinutes = absTotalSeconds `div` 60 `mod` 60
      absSeconds = absTotalSeconds `mod` 60

      signPortion = if totalSeconds < 0 then "-" else "+"
      hourPortion = (if absHours < 10 then "0" else "") <> T.pack (show absHours)
      minutePortion = (if absMinutes < 10 then ":0" else ":") <> T.pack (show absMinutes)
      secondPortion =
        if absSeconds /= 0
          then (if absSeconds < 10 then ":0" else ":") <> T.pack (show absSeconds)
          else ""
   in signPortion <> hourPortion <> minutePortion <> secondPortion

ofHours :: Int -> Either Text ZoneOffset
ofHours hours = ofHoursMinutesSeconds hours 0 0

ofHoursMinutesSeconds :: Int -> Int -> Int -> Either Text ZoneOffset
ofHoursMinutesSeconds hours minutes seconds = do
  _ <- v1
  _ <- v2
  let totalSeconds =
        (fromIntegral hours * minutesPerHour * secondsPerMinute)
          + (fromIntegral minutes * secondsPerMinute)
          + fromIntegral seconds
   in ofTotalSeconds totalSeconds
  where
    v1 :: Either Text ()
    v1 =
      if -18 <= hours && hours <= 18
        then
          if 0 < hours && (minutes < 0 || seconds < 0)
            then Left "Zone offset minutes and seconds must be positive because hours is positive"
            else
              if hours < 0 && (0 < minutes || 0 < seconds)
                then Left "Zone offset minutes and seconds must be negative because hours is negative"
                else
                  if 0 < minutes && seconds < 0 || minutes < 0 && 0 < seconds
                    then Left "Zone offset minutes and seconds must have the same sign"
                    else Right ()
        else Left $ "Zone offset hours not in valid range: value " <> T.pack (show hours) <> " is not in the range -18 to 18"
    v2 :: Either Text ()
    v2 =
      if -59 <= minutes && minutes <= 59
        then
          if -59 <= seconds && seconds <= 59
            then
              if abs hours == 18 && minutes .|. seconds /= 0
                then Left "Zone offset not in valid range: -18:00 to +18:00"
                else Right ()
            else Left $ "Zone offset seconds not in valid range: value " <> T.pack (show seconds) <> " is not in the range -59 to 59"
        else Left $ "Zone offset minutes not in valid range: value " <> T.pack (show minutes) <> " is not in the range -59 to 59"

ofTotalSeconds :: (Integral a) => a -> Either Text ZoneOffset
ofTotalSeconds totalSeconds =
  if -64800 <= totalSeconds && totalSeconds <= 64800
    then Right . ZoneOffset $ fromIntegral totalSeconds
    else Left "Zone offset not in valid range: -18:00 to +18:00"
