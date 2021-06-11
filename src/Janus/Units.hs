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
    EpochSecond (..)
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Prelude
import Janus.Units.Month (Month)
import Janus.Units.Year (Year, mkYear)

secondsPerMinute :: Int64
secondsPerMinute = 60

secondsPerHour :: Int64
secondsPerHour = secondsPerMinute * minutesPerHour

secondsPerDay :: Int64
secondsPerDay = secondsPerHour * hoursPerDay

hoursPerDay :: Int64
hoursPerDay = 24

minutesPerHour :: Int64
minutesPerHour = 60

nanosPerSecond :: Int64
nanosPerSecond = 1000_000_000

nanosPerMinute :: Int64
nanosPerMinute = nanosPerSecond * secondsPerMinute

nanosPerHour :: Int64
nanosPerHour = nanosPerMinute * minutesPerHour

nanosPerDay :: Int64
nanosPerDay = nanosPerHour * hoursPerDay

-- A date-based amount of time in the ISO-8601 calendar system, such as '2 years, 3 months and 4 days'.
data Period = Period

newtype Day = Day Int
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

mkDay :: Int -> Maybe Day
mkDay day = if 1 <= day && day <= 31 then Just (Day day) else Nothing

newtype Hour = Hour Int
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

mkHour :: Int -> Maybe Hour
mkHour hour = if 0 <= hour && hour <= 23 then Just (Hour hour) else Nothing

newtype Minute = Minute Int
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

mkMinute :: Int -> Maybe Minute
mkMinute minute = if 0 <= minute && minute <= 59 then Just (Minute minute) else Nothing

newtype Second = Second Int
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

mkSecond :: Int -> Maybe Second
mkSecond second = if 0 <= second && second <= 59 then Just (Second second) else Nothing

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

newtype Nano = Nano Int
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

mkNano :: Int -> Maybe Nano
mkNano nano = if 0 <= nano && nano <= 999_999_999 then Just (Nano nano) else Nothing

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