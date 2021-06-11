module Janus.Units.ChronoUnit
  ( ChronoUnit (..),
    length,
  )
where

import Data.Ix (Ix)
import Janus.Duration (Duration)
import qualified Janus.Duration as Duration
import Prelude hiding (length)

data ChronoUnit
  = Nanos
  | Micros
  | Millis
  | Seconds
  | Minutes
  | Hours
  | HalfDays
  | Days
  | Weeks
  | Months
  | Years
  | Decades
  | Centuries
  | Millennia
  | Eras
  | Forever
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

length :: ChronoUnit -> Duration
length Nanos = Duration.ofNanos 1
length Micros = Duration.ofNanos 1000
length Millis = Duration.ofNanos 1_000_000
length Seconds = Duration.ofSeconds 1
length Minutes = Duration.ofSeconds 60
length Hours = Duration.ofSeconds 3600
length HalfDays = Duration.ofSeconds 43_200
length Days = Duration.ofSeconds 86_400
length Weeks = Duration.ofSeconds 604_800
length Months = Duration.ofSeconds 2_629_746
length Years = Duration.ofSeconds 31_556_952
length Decades = Duration.ofSeconds 315_569_520
length Centuries = Duration.ofSeconds 3_155_695_200
length Millennia = Duration.ofSeconds 31_556_952_000
length Eras = Duration.ofSeconds 31_556_952_000_000_000
length Forever = Duration.ofSeconds 9_223_372_036_854_775_807
