module Janus.Units
  ( secondsPerMinute,
    secondsPerHour,
    secondsPerDay,
    hoursPerDay,
    minutesPerHour,
    nanosPerSecond,
    nanosPerMinute,
    nanosPerHour,
    nanosPerDay,
    Year,
    mkYear,
    Month,
    Day,
    mkDay,
    Hour,
    mkHour,
    Minute,
    mkMinute,
    Second,
    mkSecond,
    Nano,
    mkNano,
    YearMonth,
    DayOfWeek (..),
    MonthDay,
    Period,
    EpochSecond,
    EpochDay,
    Unit (..),
  )
where

import Data.Int (Int64)
import Janus.Units.Day (Day, mkDay)
import Janus.Units.DayOfWeek (DayOfWeek (..))
import Janus.Units.EpochDay (EpochDay)
import Janus.Units.EpochSecond (EpochSecond)
import Janus.Units.Hour (Hour, mkHour)
import Janus.Units.Minute (Minute, mkMinute)
import Janus.Units.Month (Month)
import Janus.Units.MonthDay (MonthDay)
import Janus.Units.Nano (Nano, mkNano)
import Janus.Units.Period (Period)
import Janus.Units.Second (Second, mkSecond)
import Janus.Units.Year (Year, mkYear)
import Janus.Units.YearMonth (YearMonth)
import Prelude

data Unit
  = Day Day
  | DayOfWeek DayOfWeek
  | EpochSecond EpochSecond
  | Hour Hour
  | Minute Minute
  | Month Month
  | MonthDay MonthDay
  | Second Second
  | Nano Nano
  | Period Period
  | Year Year
  | YearMonth YearMonth

secondsPerMinute :: Int64
secondsPerMinute = 60

-- >>> secondsPerHour
-- 3600
secondsPerHour :: Int64
secondsPerHour = secondsPerMinute * minutesPerHour

-- >>> secondsPerDay
-- 86400
secondsPerDay :: Int64
secondsPerDay = secondsPerHour * hoursPerDay

hoursPerDay :: Int64
hoursPerDay = 24

minutesPerHour :: Int64
minutesPerHour = 60

nanosPerSecond :: Int64
nanosPerSecond = 1000_000_000

-- >>> nanosPerMinute
-- 60000000000
nanosPerMinute :: Int64
nanosPerMinute = nanosPerSecond * secondsPerMinute

-- >>> nanosPerHour
-- 3600000000000
nanosPerHour :: Int64
nanosPerHour = nanosPerMinute * minutesPerHour

-- >>> nanosPerDay
-- 86400000000000
nanosPerDay :: Int64
nanosPerDay = nanosPerHour * hoursPerDay
