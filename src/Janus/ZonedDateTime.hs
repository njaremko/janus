module Janus.ZonedDateTime (ZonedDateTime, mkZonedDateTime, now) where

import Janus.LocalDateTime (LocalDateTime)
import qualified Janus.LocalDateTime as LocalDateTime
import Janus.TimeZone (TimeZone)
import Prelude

-- A date-time with a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00 Europe/Paris.
data ZonedDateTime = ZonedDateTime
  { dateTime :: LocalDateTime,
    timeZone :: TimeZone
  }

mkZonedDateTime :: LocalDateTime -> TimeZone -> ZonedDateTime
mkZonedDateTime = ZonedDateTime

now :: TimeZone -> IO ZonedDateTime
now zone = do
  dateTime <- LocalDateTime.now
  pure $ ZonedDateTime dateTime zone