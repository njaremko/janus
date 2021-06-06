-- |
-- Copyright: (c) 2021 Nathan Jaremko
-- SPDX-License-Identifier: MIT
-- Maintainer: Nathan Jaremko <nathan@jaremko.ca>
--
-- See README for more info
module Janus
  ( someFunc,
  )
where

import Prelude

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

-- An instantaneous point on the time-line.
data Instant = Instant

-- A date without a time-zone in the ISO-8601 calendar system, such as 2007-12-03.
data LocalDate = LocalDate

-- A date-time without a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30.
data LocalDateTime = LocalDateTime
  { date :: LocalDate,
    time :: LocalTime
  }

-- A time without a time-zone in the ISO-8601 calendar system, such as 10:15:30.
data LocalTime = LocalTime

-- A time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 10:15:30+01:00.
data OffsetTime = OffsetTime

-- A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00.
data OffsetDateTime = OffsetDateTime

-- A date-time with a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00 Europe/Paris.
data ZonedDateTime = ZonedDateTime

-- A time-zone, such as Europe/Paris.
data TimeZone = TimeZone

-- A time-zone offset from Greenwich/UTC, such as +02:00.
data TimeOffset = TimeOffest

-- A date-based amount of time in the ISO-8601 calendar system, such as '2 years, 3 months and 4 days'.
data Period = Period

-- A year in the ISO-8601 calendar system, such as 2007.
data Year = Year

-- A year-month in the ISO-8601 calendar system, such as 2007-12.
data YearMonth = YearMonth

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