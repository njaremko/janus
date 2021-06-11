module Janus.Units.Year (
    Year, mkYear, toInt, length, isLeapYear,unsafeMkYear
    ) where

import Data.Bits (Bits, (.&.))
import Data.Ix (Ix)
import Prelude hiding (length)
import Data.Maybe (fromMaybe)

-- A year in the ISO-8601 calendar system, such as 2007.
newtype Year = Year Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord,
      Real,
      Bits
    )

mkYear :: (Integral a) => a -> Maybe Year
mkYear year = if -999999999 <= year && year <= 999999999 then Just (Year $ fromIntegral year) else Nothing

unsafeMkYear :: (Integral a) => a -> Year
unsafeMkYear year = fromMaybe (error "") $ mkYear year

toInt :: (Integral a) => Year -> a
toInt (Year a) = fromIntegral a

length :: Year -> Int
length y = if isLeapYear y then 366 else 365

-- >>> isLeapYear 2019
-- >>> isLeapYear 2020
-- False
-- True
isLeapYear :: Year -> Bool
isLeapYear prolepticYear =
  prolepticYear .&. 3 == 0 && (toInt @Int prolepticYear `mod` 100 /= 0 || toInt @Int prolepticYear `mod` 400 == 0)
