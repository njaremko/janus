module Janus.LocalDate
  ( LocalDate,
    mkLocalDate,
    ofYearDay,
    withDay,
    withMonth,
    withYear,
    toEpochDay,
    ofEpochDay,
    lengthOfMonth,
    plusDays,
    plusWeeks,
    plusMonths,
    plusYears,
  )
where

import Data.Int (Int64)
import Janus.Units
import qualified Janus.Units.Day as Day
import qualified Janus.Units.Month as Month
import qualified Janus.Units.Year as Year
import Prelude
import Data.Ix (Ix)

-- A date without a time-zone in the ISO-8601 calendar system, such as 2007-12-03.
data LocalDate = LocalDate
  { year :: Year,
    month :: Month,
    day :: Day
  } deriving stock (Show, Eq, Ord, Bounded, Ix)

mkLocalDate :: Year -> Month -> Day -> LocalDate
mkLocalDate = LocalDate

ofYearDay :: Year -> Int -> Maybe LocalDate
ofYearDay year dayOfYear = do
  moy <- Month.fromOrdinal ((dayOfYear - 1) `div` 31 + 1)
  let isLeap = Year.isLeapYear year
      monthEnd = Month.monthStartDayOfYear isLeap moy + Month.length isLeap moy - 1
      adjustedMOY = if monthEnd < dayOfYear then Month.plus @Int 1 moy else moy
      dom = dayOfYear - Month.monthStartDayOfYear isLeap adjustedMOY + 1
  return LocalDate {year, month = moy, day = fromIntegral dom}

withDay :: Day -> LocalDate -> LocalDate
withDay day date = date {day}

withMonth :: Month -> LocalDate -> LocalDate
withMonth month date = date {month}

withYear :: Year -> LocalDate -> LocalDate
withYear year date = date {year}

plusDays :: (Integral a) => a -> LocalDate -> LocalDate
plusDays 0 date = date
plusDays days date@LocalDate {year, month, day} =
  let dom = Day.toInt day + fromIntegral days
   in if dom < 59
        then
          let monthLen = lengthOfMonth date
           in if dom < monthLen
                then date {day = fromIntegral dom}
                else
                  if Month.toOrdinal @Int month < 12
                    then date {month = Month.plus @Int 1 month, day = fromIntegral (dom - monthLen)}
                    else date {year = year + 1, month = Month.January, day = fromIntegral (dom - monthLen)}
        else ofEpochDay (toEpochDay date + fromIntegral days)

plusWeeks :: Int -> LocalDate -> LocalDate
plusWeeks weeks = plusDays (weeks * 7)

plusMonths :: Int -> LocalDate -> LocalDate
plusMonths 0 d = d
plusMonths months LocalDate {month, day, year} =
  let monthCount = Year.toInt year * 12 + Month.toOrdinal month - 1
      calcMonths = monthCount + months
      newYear = calcMonths `div` 12
      newMonth = calcMonths `mod` 12 + 1
   in resolvePreviousValid (fromIntegral newYear) (Month.unsafeFromOrdinal newMonth) day

plusYears :: Int -> LocalDate -> LocalDate
plusYears years date@LocalDate {year} = date {year = year + fromIntegral years}

resolvePreviousValid :: Year -> Month -> Day -> LocalDate
resolvePreviousValid y m d =
  let newDayNum :: Int = min (Day.toInt d) (Month.length (Year.isLeapYear y) m)
      newDay = case mkDay newDayNum of
        Just day -> day
        Nothing -> error $ "Failed to resolve previously valid day: " <> show newDayNum
   in LocalDate y m newDay

-- >>> toEpochDay $ LocalDate 2020 Month.February 29
-- 18321
-- >>> toEpochDay $ LocalDate (-500) Month.January 5
-- -902145
toEpochDay :: LocalDate -> Int64
toEpochDay LocalDate {year, month, day} =
  let y :: Int = Year.toInt year
      x1 = 365 * y
      x2 =
        if 0 <= y
          then x1 + ((y + 3) `div` 4 - (y + 99) `div` 100 + (y + 399) `div` 400)
          else x1 - (y `div` (-4) - y `div` (-100) + y `div` (-400))
      x3 = x2 + ((367 * Month.toOrdinal month - 362) `div` 12)
      x4 = x3 + (Day.toInt day - 1)
      x5 =
        if 2 < Month.toOrdinal @Int month
          then
            if Year.isLeapYear year
              then x4 - 1
              else x4 - 2
          else x4
   in fromIntegral x5 - 719528

ofEpochDay :: Int64 -> LocalDate
ofEpochDay epochDay =
  let zeroDay = epochDay + 719528 - 60
      (yearEst, adjust, zeroDay2) =
        if zeroDay < 0
          then ((zeroDay + 1) `div` 146097 - 1, yearEst * 400, zeroDay + (- yearEst) * 146097)
          else (0, 0, zeroDay)
      yearEst2 = (400 * zeroDay2 + 591) `div` 146097
      doyEst = zeroDay2 - (365 * yearEst2 + yearEst2 `div` 4 - yearEst2 `div` 100 + yearEst2 `div` 400)
      (yearEst3, doyEst2) =
        if doyEst < 0
          then (yearEst2 - 1, zeroDay2 - (365 * yearEst2 + yearEst2 `div` 4 - yearEst2 `div` 100 + yearEst2 `div` 400))
          else (yearEst2, doyEst)
      yearEst4 = yearEst3 + adjust
      marchDoy0 = doyEst2
      marchMonth0 = (marchDoy0 * 5 + 2) `div` 153
      month = Month.unsafeFromOrdinal $ (marchMonth0 + 2) `mod` 12 + 1
      dom = Day.unsafeMkDay $ marchDoy0 - (marchMonth0 * 306 + 5) `div` 10 + 1
      yearEst5 = yearEst4 + (marchMonth0 `div` 10)
      year = Year.unsafeMkYear yearEst5
   in LocalDate year month dom

lengthOfMonth :: LocalDate -> Int
lengthOfMonth LocalDate {year, month} = Month.length (Year.isLeapYear year) month
