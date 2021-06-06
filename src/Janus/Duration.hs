{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Janus.Duration
  ( Duration (..),
    ofDays,
    ofSeconds,
    ofSecondsWithAdjustment,
    ofHours,
    ofMinutes,
    ofMillis,
    ofNanos,
  )
where

import Data.Int (Int64)
import Janus.Units
    ( secondsPerMinute, secondsPerHour, secondsPerDay, nanosPerSecond )
import Prelude

-- A time-based amount of time, such as '34.5 seconds'.
data Duration = Duration
  { -- The number of seconds in the duration.
    seconds :: Int64,
    -- The number of nanoseconds in the duration, expressed as a fraction of the
    -- number of seconds. This is always positive, and never exceeds 999,999,999.
    nanos :: Int
  }

ofDays :: Int64 -> Duration
ofDays days =
  Duration
    { seconds = secondsPerDay * days,
      nanos = 0
    }

ofHours :: Int64 -> Duration
ofHours hours =
  Duration
    { seconds = secondsPerHour * hours,
      nanos = 0
    }

ofMinutes :: Int64 -> Duration
ofMinutes minutes =
  Duration
    { seconds = secondsPerMinute * minutes,
      nanos = 0
    }

ofSeconds :: Int64 -> Duration
ofSeconds seconds =
  Duration
    { seconds,
      nanos = 0
    }

ofSecondsWithAdjustment :: Int64 -> Int64 -> Duration
ofSecondsWithAdjustment initialSeconds nanoAdjustment =
  let seconds = initialSeconds + floor (realToFrac @Int64 @Double nanoAdjustment / realToFrac nanosPerSecond)
      nanos :: Int = fromIntegral (nanoAdjustment `mod` nanosPerSecond)
   in Duration
        { seconds,
          nanos
        }

ofMillis :: Int64 -> Duration
ofMillis millis =
  let mos = fromIntegral (millis `mod` 1000)
      secs = millis `div` 1000
   in if mos < 0
        then
          Duration
            { seconds = secs - 1,
              nanos = (mos + 1000) * 1000_000
            }
        else
          Duration
            { seconds = secs,
              nanos = mos * 1000_000
            }

ofNanos :: Int64 -> Duration
ofNanos nanos =
  let secs = nanos `div` nanosPerSecond
      nos :: Int = fromIntegral (nanos `mod` nanosPerSecond)
   in if nos < 0
        then
          Duration
            { seconds = secs - 1,
              nanos = nos + fromIntegral nanosPerSecond
            }
        else
          Duration
            { seconds = secs,
              nanos = nos
            }