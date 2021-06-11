module Janus.Units.Year where

import Prelude hiding (length)
import Data.Ix (Ix)
import Data.Bits ((.&.), Bits)

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
      Integral,
      Bits
    )

mkYear :: Int -> Maybe Year
mkYear year = if year < 0 then Nothing else Just (Year year)

length :: Year -> Int
length y = if isLeapYear y then 366 else 365

-- >>> isLeapYear 2019
-- >>> isLeapYear 2020
-- False
-- True
isLeapYear :: Year -> Bool
isLeapYear prolepticYear =
  prolepticYear .&. 3 == 0 && (prolepticYear `mod` 100 /= 0 || prolepticYear `mod` 400 == 0)
