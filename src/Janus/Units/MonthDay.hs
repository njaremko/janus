module Janus.Units.MonthDay (MonthDay) where

import Prelude 
import Janus.Units.Month (Month)
import Data.Ix (Ix)


-- A month-day in the ISO-8601 calendar system, such as --12-03.
data MonthDay = MonthDay
  { month :: Month,
    day :: Int
  }
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )