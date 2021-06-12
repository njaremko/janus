module Janus.Units.YearMonth (YearMonth) where

import Janus.Units.Month (Month)
import Janus.Units.Year (Year)
import Prelude
import Data.Ix (Ix)

-- A year-month in the ISO-8601 calendar system, such as 2007-12.
data YearMonth = YearMonth
  { year :: Year,
    month :: Month
  }
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )