module Janus.Units.Month
  ( Month (..),
    toOrdinal,
    fromOrdinal,
    monthStartDayOfYear,
    minLength,
    maxLength,
    length,
    plus,
    minus,
    unsafeFromOrdinal
  )
where

import Data.Ix (Ix)
import Prelude hiding (length)
import Data.Text (Text)
import qualified Data.Text as T

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
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

toOrdinal :: (Integral a) => Month -> a
toOrdinal January = 1
toOrdinal February = 2
toOrdinal March = 3
toOrdinal April = 4
toOrdinal May = 5
toOrdinal June = 6
toOrdinal July = 7
toOrdinal August = 8
toOrdinal September = 9
toOrdinal October = 10
toOrdinal November = 11
toOrdinal December = 12

fromOrdinal :: (Integral a, Show a) => a -> Either Text Month
fromOrdinal 1 = Right January
fromOrdinal 2 = Right February
fromOrdinal 3 = Right March
fromOrdinal 4 = Right April
fromOrdinal 5 = Right May
fromOrdinal 6 = Right June
fromOrdinal 7 = Right July
fromOrdinal 8 = Right August
fromOrdinal 9 = Right September
fromOrdinal 10 = Right October
fromOrdinal 11 = Right November
fromOrdinal 12 = Right December
fromOrdinal idx = Left $ "Month index " <> T.pack (show idx) <> " invalid"

unsafeFromOrdinal :: (Integral a, Show a) => a -> Month
unsafeFromOrdinal idx =
  case fromOrdinal idx of
    Right m -> m
    Left err -> error $ T.unpack err

monthStartDayOfYear :: (Integral a) => Bool -> Month -> a
monthStartDayOfYear isLeapYear month =
  let leapYearAdjustment = if isLeapYear then 1 else 0
   in firstDayOfMonthOfYear month leapYearAdjustment
  where
    firstDayOfMonthOfYear January _ = 1
    firstDayOfMonthOfYear February _ = 32
    firstDayOfMonthOfYear March x = 60 + x
    firstDayOfMonthOfYear April x = 91 + x
    firstDayOfMonthOfYear May x = 121 + x
    firstDayOfMonthOfYear June x = 152 + x
    firstDayOfMonthOfYear July x = 182 + x
    firstDayOfMonthOfYear August x = 213 + x
    firstDayOfMonthOfYear September x = 244 + x
    firstDayOfMonthOfYear October x = 274 + x
    firstDayOfMonthOfYear November x = 305 + x
    firstDayOfMonthOfYear December x = 335 + x

minLength :: (Integral a) =>  Month -> a
minLength February = 28
minLength April = 30
minLength June = 30
minLength September = 30
minLength November = 30
minLength _ = 31

maxLength :: (Integral a) =>  Month -> a
maxLength February = 29
maxLength April = 30
maxLength June = 30
maxLength September = 30
maxLength November = 30
maxLength _ = 31

length :: (Integral a) => Bool -> Month -> a
length isLeapYear February = if isLeapYear then 29 else 28
length _ April = 30
length _ June = 30
length _ September = 30
length _ November = 30
length _ _ = 31

plus :: (Integral a, Show a) => a -> Month -> Month
plus toAdd month =
  let amount = toAdd `mod` 12
   in unsafeFromOrdinal ((toOrdinal month + amount + 12) `mod` 12)

minus :: (Integral a, Show a) => a -> Month -> Month
minus toSub = plus (- (toSub `mod` 12))