module Janus.Units.Year
  ( Year,
    mkYear,
    toInt,
    length,
    isLeapYear,
    unsafeMkYear,
  )
where

import Data.Bits (Bits, (.&.))
import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (length)

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
      Bits
    )

mkYear :: (Integral a, Show a) => a -> Either Text Year
mkYear year =
  if -999999999 <= year && year <= 999999999
    then Right (Year $ fromIntegral year)
    else Left $ "Given value " <> T.pack (show year) <> " isn't a valid year"

unsafeMkYear :: (Integral a, Show a) => a -> Year
unsafeMkYear year = case mkYear year of
  Right a -> a
  Left err -> error $ T.unpack err

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
