module Janus.Units where

import Prelude
import Data.Int (Int64)

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
data Year = Year

-- A year-month in the ISO-8601 calendar system, such as 2007-12.
data YearMonth = YearMonth {
       year :: Year,
       month :: Month
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

-- A day-of-week, such as 'Tuesday'.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday