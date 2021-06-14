module Janus.Units.DayOfWeek
  ( DayOfWeek (..),
    mkDayOfWeek,
    unsafeMkDayOfWeek,
  )
where

import Data.Ix (Ix)
import Data.Text (Text)
import Prelude

-- A day-of-week, such as 'Tuesday'.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

mkDayOfWeek :: (Integral a) => a -> Either Text DayOfWeek
mkDayOfWeek dayOfWeek = case dayOfWeek of
  1 -> Right Monday
  2 -> Right Tuesday
  3 -> Right Wednesday
  4 -> Right Thursday
  5 -> Right Friday
  6 -> Right Saturday
  7 -> Right Sunday
  _ -> Left ""

unsafeMkDayOfWeek :: (Integral a) => a -> DayOfWeek
unsafeMkDayOfWeek dayOfWeek = case dayOfWeek of
  1 ->  Monday
  2 ->  Tuesday
  3 ->  Wednesday
  4 ->  Thursday
  5 ->  Friday
  6 ->  Saturday
  7 ->  Sunday
  _ -> error ""