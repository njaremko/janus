module Janus.LocalDate where

import Data.Bits ((.&.))
import Janus.Units
import qualified Janus.Units.Month as Month
import Prelude

-- A date without a time-zone in the ISO-8601 calendar system, such as 2007-12-03.
data LocalDate = LocalDate
  { year :: Int,
    month :: Month,
    day :: Int
  }

ofYearDay :: Int -> Int -> Maybe LocalDate
ofYearDay year dayOfYear = do
  moy <- Month.fromOrdinal ((dayOfYear - 1) `div` 31 + 1)
  let isLeap = isLeapYear year
      monthEnd = Month.monthStartDayOfYear moy isLeap + Month.length moy isLeap - 1
      adjustedMOY = if monthEnd < dayOfYear then Month.plus moy 1 else moy
      dom = dayOfYear - Month.monthStartDayOfYear adjustedMOY isLeap + 1
  return LocalDate {year, month = moy, day = dom}

-- >>> isLeapYear 2019
-- >>> isLeapYear 2020
-- False
-- True
isLeapYear :: Int -> Bool
isLeapYear prolepticYear =
  prolepticYear .&. 3 == 0 && (prolepticYear `mod` 100 /= 0 || prolepticYear `mod` 400 == 0)
