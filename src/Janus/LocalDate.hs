module Janus.LocalDate
  ( LocalDate,
    mkLocalDate,
    ofYearDay,
    withDay,
    withMonth,
    withYear,
  )
where

import Data.Int (Int64)
import Janus.Duration (Duration)
import Janus.Units
import qualified Janus.Units.Month as Month
import Janus.Units.Year (isLeapYear)
import Prelude

-- A date without a time-zone in the ISO-8601 calendar system, such as 2007-12-03.
data LocalDate = LocalDate
  { year :: Year,
    month :: Month,
    day :: Day
  }

mkLocalDate :: Year -> Month -> Day -> LocalDate
mkLocalDate = LocalDate

ofYearDay :: Year -> Int -> Maybe LocalDate
ofYearDay year dayOfYear = do
  moy <- Month.fromOrdinal ((dayOfYear - 1) `div` 31 + 1)
  let isLeap = isLeapYear year
      monthEnd = Month.monthStartDayOfYear isLeap moy + Month.length isLeap moy - 1
      adjustedMOY = if monthEnd < dayOfYear then Month.plus 1 moy else moy
      dom = dayOfYear - Month.monthStartDayOfYear isLeap adjustedMOY + 1
  return LocalDate {year, month = moy, day = fromIntegral dom}

withDay :: Day -> LocalDate -> LocalDate
withDay day date = date {day}

withMonth :: Month -> LocalDate -> LocalDate
withMonth month date = date {month}

withYear :: Year -> LocalDate -> LocalDate
withYear year date = date {year}

plusDays :: Int -> LocalDate -> LocalDate
plusDays 0 date = date
plusDays days date@LocalDate {year, month, day} =
  let dom = fromIntegral day + days
   in if dom < 59
        then
          let monthLen = lengthOfMonth date
           in if dom < monthLen
                then date {day = fromIntegral dom}
                else
                  if Month.toOrdinal month < 12
                    then date {month = Month.plus 1 month, day = fromIntegral (dom - monthLen)}
                    else date {year = year + 1, month = Month.January, day = fromIntegral (dom - monthLen)}
        else ofEpochDay (toEpochDay date + fromIntegral days)

-- >>> toEpochDay $ LocalDate 2020 Month.February 29
-- 18321
-- >>> toEpochDay $ LocalDate (-500) Month.January 5
-- -902145
toEpochDay :: LocalDate -> Int64
toEpochDay LocalDate {year = y, month, day} =
  let x1 = 365 * y
      x2 =
        if 0 <= y
          then x1 + ((y + 3) `div` 4 - (y + 99) `div` 100 + (y + 399) `div` 400)
          else x1 - (y `div` (-4) - y `div` (-100) + y `div` (-400))
      x3 = fromIntegral x2 + ((367 * Month.toOrdinal month - 362) `div` 12)
      x4 = x3 + (fromIntegral day - 1)
      x5 =
        if 2 < Month.toOrdinal month
          then
            if isLeapYear y
              then x4 - 1
              else x4 - 2
          else x4
   in fromIntegral x5 - 719528

ofEpochDay :: Int64 -> LocalDate
ofEpochDay epochDay =
  let
   in LocalDate 0 Month.January 0

lengthOfMonth :: LocalDate -> Int
lengthOfMonth LocalDate {year, month} = Month.length (isLeapYear year) month
