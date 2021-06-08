module Janus.Units where

import Data.Int (Int64)
import Prelude

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

-- A year in the ISO-8601 calendar system, such as 2007.
newtype Year = Year {unYear :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)
newtype Day = Day {unDay :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)
newtype Hour = Hour {unHour :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)
newtype Minute = Minute {unMinute :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)
newtype Second = Second {unSecond :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)
newtype Nano = Nano {unNano :: Int} deriving newtype (Enum, Num, Eq, Ord, Real, Integral)

-- A year-month in the ISO-8601 calendar system, such as 2007-12.
data YearMonth = YearMonth
  { year :: Year,
    month :: Month
  }

-- A month-day in the ISO-8601 calendar system, such as --12-03.
data MonthDay = MonthDay
  { month :: Month,
    day :: Int
  }

-- A month-of-year, such as 'July'.
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

monthToInt :: Month -> Int
monthToInt January = 1
monthToInt February = 2
monthToInt March = 3
monthToInt April = 4
monthToInt May = 5
monthToInt June = 6
monthToInt July = 7
monthToInt August = 8
monthToInt September = 9
monthToInt October = 10
monthToInt November = 11
monthToInt December = 12


-- A day-of-week, such as 'Tuesday'.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

data ChronoUnit
  = Nanos Int64
  | Micros Int64
  | Millis Int64
  | Seconds Int64
  | Minutes Int64
  | Hours Int64