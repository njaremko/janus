module Janus.LocalDateTime (LocalDateTime) where

import Prelude
import Janus.LocalDate (LocalDate)
import Janus.LocalTime (LocalTime)
import Data.Ix (Ix)

-- A date-time without a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30.
data LocalDateTime = LocalDateTime
  { date :: LocalDate,
    time :: LocalTime
  } deriving stock (Show, Eq, Ord, Bounded, Ix)