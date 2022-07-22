module Janus.OffsetDateTime
  ( OffsetDateTime,
    mkOffsetDateTime,
    getYear,
    getMonth,
    getDayOfMonth,
    getHour,
    getMinute,
    getSecond,
    getNano,
    now,
  )
where

import Data.Ix (Ix)
import Janus.LocalDateTime (LocalDateTime)
import qualified Janus.LocalDateTime as LocalDateTime
import Janus.Units
import Janus.ZoneOffset (ZoneOffset)
import Prelude

-- A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00.
data OffsetDateTime = OffsetDateTime
  { dateTime :: LocalDateTime,
    offset :: ZoneOffset
  }
  deriving stock (Show, Eq, Ord, Bounded, Ix)

mkOffsetDateTime :: LocalDateTime -> ZoneOffset -> OffsetDateTime
mkOffsetDateTime = OffsetDateTime

now :: ZoneOffset -> IO OffsetDateTime
now offset = do
  dateTime <- LocalDateTime.now
  pure $ OffsetDateTime dateTime offset

getYear :: OffsetDateTime -> Year
getYear OffsetDateTime {dateTime} = LocalDateTime.getYear dateTime

getMonth :: OffsetDateTime -> Month
getMonth OffsetDateTime {dateTime} = LocalDateTime.getMonth dateTime

getDayOfMonth :: OffsetDateTime -> Day
getDayOfMonth OffsetDateTime {dateTime} = LocalDateTime.getDayOfMonth dateTime

getHour :: OffsetDateTime -> Hour
getHour OffsetDateTime {dateTime} = LocalDateTime.getHour dateTime

getMinute :: OffsetDateTime -> Minute
getMinute OffsetDateTime {dateTime} = LocalDateTime.getMinute dateTime

getSecond :: OffsetDateTime -> Second
getSecond OffsetDateTime {dateTime} = LocalDateTime.getSecond dateTime

getNano :: OffsetDateTime -> Nano
getNano OffsetDateTime {dateTime} = LocalDateTime.getNano dateTime