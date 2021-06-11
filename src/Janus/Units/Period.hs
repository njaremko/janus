module Janus.Units.Period
  ( Period,
    mkPeriod,
    ofYears,
    ofMonths,
    ofDays,
    getYears,
    getMonths,
    getDays,
    plusDays,
    plusMonths,
    plusYears,
    minusDays,
    minusMonths,
    minusYears,
    multipliedBy,
    negate,
    toTotalMonths,
    normalize,
  )
where

import Data.Int (Int64)
import Prelude hiding (negate)

-- A date-based amount of time in the ISO-8601 calendar system, such as '2 years, 3 months and 4 days'.
data Period = Period
  { years :: Int,
    months :: Int,
    days :: Int
  }

instance Semigroup Period where
  (<>)
    Period {years = y1, months = m1, days = d1}
    Period {years = y2, months = m2, days = d2} =
      Period {years = y1 + y2, months = m1 + m2, days = d1 + d2}

instance Monoid Period where
  mempty = Period 0 0 0

mkPeriod :: Int -> Int -> Int -> Period
mkPeriod = Period

ofYears :: Int -> Period
ofYears years = Period {years, months = 0, days = 0}

ofMonths :: Int -> Period
ofMonths months = Period {years = 0, months, days = 0}

ofDays :: Int -> Period
ofDays days = Period {years = 0, months = 0, days}

getYears :: Period -> Int
getYears = years

getMonths :: Period -> Int
getMonths = months

getDays :: Period -> Int
getDays = days

plusDays :: Int -> Period -> Period
plusDays 0 p = p
plusDays daysToAdd p@Period {days} = p {days = days + daysToAdd}

plusMonths :: Int -> Period -> Period
plusMonths 0 p = p
plusMonths monthsToAdd p@Period {months} = p {months = months + monthsToAdd}

plusYears :: Int -> Period -> Period
plusYears 0 p = p
plusYears yearsToAdd p@Period {years} = p {years = years + yearsToAdd}

minusDays :: Int -> Period -> Period
minusDays (-9223372036854775808) p = plusDays 1 (plusDays 9223372036854775807 p)
minusDays daysToSub p = plusDays (- daysToSub) p

minusMonths :: Int -> Period -> Period
minusMonths (-9223372036854775808) p = plusMonths 1 (plusMonths 9223372036854775807 p)
minusMonths monthsToSub p = plusMonths (- monthsToSub) p

minusYears :: Int -> Period -> Period
minusYears (-9223372036854775808) p = plusYears 1 (plusYears 9223372036854775807 p)
minusYears yearsToSub p = plusYears (- yearsToSub) p

multipliedBy :: Int -> Period -> Period
multipliedBy 1 p = p
multipliedBy _ Period {years = 0, months = 0, days = 0} = mempty
multipliedBy scalar Period {years, months, days} =
  Period
    { years = years * scalar,
      months = months * scalar,
      days = days * scalar
    }

negate :: Period -> Period
negate = multipliedBy (-1)

toTotalMonths :: Period -> Int64
toTotalMonths Period {years, months} = fromIntegral $ years * 12 + months

normalize :: Period -> Period
normalize p@Period {years, months} =
  let totalMonths = toTotalMonths p
      splitYears = totalMonths `div` 12
      splitMonths :: Int = fromIntegral $ totalMonths `mod` 12
   in if splitYears == fromIntegral years && splitMonths == months
        then p
        else p {years = fromIntegral splitYears, months = splitMonths}