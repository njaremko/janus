module Janus.Units.ChronoField
  ( ChronoField (..),
    range,
    baseUnit,
    rangeUnit
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Janus.Units.ChronoUnit (ChronoUnit (..))
import Prelude

data ChronoField
  = NanoOfSecond
  | NanoOfDay
  | MicroOfSecond
  | MicroOfDay
  | MilliOfSecond
  | MilliOfDay
  | SecondOfMinute
  | SecondOfDay
  | MinuteOfHour
  | MinuteOfDay
  | HourOfAmPm
  | ClockHourOfAmPm
  | HourOfDay
  | ClockHourOfDay
  | AmPmOfDay
  | DayOfWeek
  | AlignedDayOfWeekInMonth
  | AlignedDayOfWeekInYear
  | DayOfMonth
  | DayOfYear
  | EpochDay
  | AlignedWeekOfMonth
  | AlignedWeekOfYear
  | MonthOfYear
  | ProlepticMonth
  | YearOfEra
  | Year
  | Era
  | InstantSeconds
  | OffsetSeconds
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

baseUnit :: ChronoField -> ChronoUnit
baseUnit NanoOfSecond = Nanos
baseUnit NanoOfDay = Nanos
baseUnit MicroOfSecond = Micros
baseUnit MicroOfDay = Micros
baseUnit MilliOfSecond = Millis
baseUnit MilliOfDay = Millis
baseUnit SecondOfMinute = Seconds
baseUnit SecondOfDay = Seconds
baseUnit MinuteOfHour = Minutes
baseUnit MinuteOfDay = Minutes
baseUnit HourOfAmPm = Hours
baseUnit ClockHourOfAmPm = Hours
baseUnit HourOfDay = Hours
baseUnit ClockHourOfDay = Hours
baseUnit AmPmOfDay = HalfDays
baseUnit DayOfWeek = Days
baseUnit AlignedDayOfWeekInMonth = Days
baseUnit AlignedDayOfWeekInYear = Days
baseUnit DayOfMonth = Days
baseUnit DayOfYear = Days
baseUnit EpochDay = Days
baseUnit AlignedWeekOfMonth = Weeks
baseUnit AlignedWeekOfYear = Weeks
baseUnit MonthOfYear = Months
baseUnit ProlepticMonth = Months
baseUnit YearOfEra = Years
baseUnit Year = Years
baseUnit Era = Eras
baseUnit InstantSeconds = Seconds
baseUnit OffsetSeconds = Seconds

rangeUnit :: ChronoField -> ChronoUnit
rangeUnit NanoOfSecond = Seconds
rangeUnit NanoOfDay = Days
rangeUnit MicroOfSecond = Seconds
rangeUnit MicroOfDay = Days
rangeUnit MilliOfSecond = Seconds
rangeUnit MilliOfDay = Days
rangeUnit SecondOfMinute = Minutes
rangeUnit SecondOfDay = Days
rangeUnit MinuteOfHour = Hours
rangeUnit MinuteOfDay = Days
rangeUnit HourOfAmPm = HalfDays 
rangeUnit ClockHourOfAmPm = HalfDays
rangeUnit HourOfDay = Days
rangeUnit ClockHourOfDay = Days
rangeUnit AmPmOfDay = Days
rangeUnit DayOfWeek = Weeks
rangeUnit AlignedDayOfWeekInMonth = Weeks
rangeUnit AlignedDayOfWeekInYear = Weeks
rangeUnit DayOfMonth = Months
rangeUnit DayOfYear = Years
rangeUnit EpochDay = Forever
rangeUnit AlignedWeekOfMonth = Months
rangeUnit AlignedWeekOfYear = Years
rangeUnit MonthOfYear = Years
rangeUnit ProlepticMonth = Forever
rangeUnit YearOfEra = Forever 
rangeUnit Year = Forever
rangeUnit Era = Forever
rangeUnit InstantSeconds = Forever
rangeUnit OffsetSeconds = Forever

range :: ChronoField -> (Int64, Int64)
range NanoOfSecond = (0, 999999999)
range NanoOfDay = (0, 86399999999999)
range MicroOfSecond = (0, 999999)
range MicroOfDay = (0, 86399999999)
range MilliOfSecond = (0, 999)
range MilliOfDay = (0, 86399999)
range SecondOfMinute = (0, 59)
range SecondOfDay = (0, 86399)
range MinuteOfHour = (0, 59)
range MinuteOfDay = (0, 1439)
range HourOfAmPm = (0, 11)
range ClockHourOfAmPm = (1, 12)
range HourOfDay = (0, 23)
range ClockHourOfDay = (1, 24)
range AmPmOfDay = (0, 1)
range DayOfWeek = (1, 7)
range AlignedDayOfWeekInMonth = (1, 7)
range AlignedDayOfWeekInYear = (1, 7)
range DayOfMonth = (1, 31)
range DayOfYear = (1, 366)
range EpochDay = (-365243219162, 365241780471)
range AlignedWeekOfMonth = (1, 5)
range AlignedWeekOfYear = (1, 53)
range MonthOfYear = (1, 12)
range ProlepticMonth = (-11999999988, 11999999999)
range YearOfEra = (1, 1000000000)
range Year = (-999999999, 999999999)
range Era = (0, 1)
range InstantSeconds = (-9223372036854775808, 9223372036854775807)
range OffsetSeconds = (-64800, 64800)