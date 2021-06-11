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
    EpochSecond (..),
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Janus.Units.Day (Day, mkDay)
import Janus.Units.Hour (Hour, mkHour)
import Janus.Units.Minute (Minute, mkMinute)
import Janus.Units.Month (Month)
import Janus.Units.Year (Year, mkYear)
import Janus.Units.Second (Second, mkSecond)
import Janus.Units.Nano (Nano, mkNano)
import Janus.Units.Period (Period)
import Prelude

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

newtype EpochSecond = EpochSecond Int64
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord,
      Real,
      Integral
    )


-- A year-month in the ISO-8601 calendar system, such as 2007-12.
data YearMonth = YearMonth
  { year :: Year,
    month :: Month
  }
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )

-- A month-day in the ISO-8601 calendar system, such as --12-03.
data MonthDay = MonthDay
  { month :: Month,
    day :: Int
  }
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )

-- A day-of-week, such as 'Tuesday'.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )
