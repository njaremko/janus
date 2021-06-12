module Janus.LocalDateTime
  ( LocalDateTime,
    mkLocalDateTime,
    ofEpochSecond
  )
where

import Data.Ix (Ix)
import qualified Janus.LocalDate as LocalDate
import Janus.LocalDate (LocalDate)
import qualified Janus.LocalTime as LocalTime
import Janus.LocalTime (LocalTime)
import Prelude
import Data.Int (Int64)
import Janus.ZoneOffset (ZoneOffset)
import qualified Janus.ZoneOffset as ZoneOffset
import Data.Text (Text)
import qualified Janus.Units.ChronoField as  ChronoField

-- A date-time without a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30.
data LocalDateTime = LocalDateTime
  { date :: LocalDate,
    time :: LocalTime
  }
  deriving stock (Show, Eq, Ord, Bounded, Ix)

mkLocalDateTime :: LocalDate -> LocalTime -> LocalDateTime
mkLocalDateTime = LocalDateTime

ofEpochSecond :: Int64 -> Int -> ZoneOffset -> Either Text LocalDateTime 
ofEpochSecond epochSecond nanoOfSecond offset = do
  _ <- ChronoField.checkValid ChronoField.NanoOfSecond nanoOfSecond
  let localSecond = epochSecond + fromIntegral (ZoneOffset.getTotalSeconds offset)
  let localEpochDay = localSecond `div` 86400
  let secsOfDay = localSecond `mod` 86400
  let date = LocalDate.ofEpochDay localEpochDay
  let time = LocalTime.ofNanoOfDay (secsOfDay * 1000000000 + fromIntegral nanoOfSecond)
  Right $ mkLocalDateTime date time