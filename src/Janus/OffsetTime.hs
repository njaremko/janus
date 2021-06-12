module Janus.OffsetTime
  ( OffsetTime,
    mkOffsetTime,
    getHour,
    getMinute,
    getSecond,
    getNano,
    getOffset,
    withHour,
    withMinute,
    withSecond,
    withNanos,
    plusHours,
    plusMinutes,
    plusSeconds,
    plusNanos,
    minusHours,
    minusMinutes,
    minusSeconds,
    minusNanos,
    toEpochNano,
    toEpochSecond
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Janus.LocalTime (LocalTime)
import qualified Janus.LocalTime as LocalTime
import Janus.Units.Hour (Hour)
import qualified Janus.Units.Hour as Hour
import Janus.Units.Minute (Minute)
import qualified Janus.Units.Minute as Minute
import Janus.Units.Nano (Nano)
import qualified Janus.Units.Nano as Nano
import Janus.Units.Second (Second)
import qualified Janus.Units.Second as Second
import Janus.ZoneOffset (ZoneOffset)
import qualified Janus.ZoneOffset as ZoneOffset
import Prelude
import Janus.LocalDate (LocalDate)
import qualified Janus.LocalDate as LocalDate

-- A time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 10:15:30+01:00.
data OffsetTime = OffsetTime
  { time :: LocalTime,
    offset :: ZoneOffset
  }
  deriving stock
    ( Show,
      Bounded,
      Ix,
      Eq,
      Ord
    )

getHour :: OffsetTime -> Hour
getHour OffsetTime{time} = LocalTime.getHour time

getMinute :: OffsetTime -> Minute
getMinute OffsetTime{time} = LocalTime.getMinute time

getSecond :: OffsetTime -> Second
getSecond OffsetTime{time} = LocalTime.getSecond time

getNano :: OffsetTime -> Nano
getNano OffsetTime{time} = LocalTime.getNano time

getOffset :: OffsetTime -> ZoneOffset
getOffset OffsetTime{offset} = offset

mkOffsetTime :: LocalTime -> ZoneOffset -> OffsetTime
mkOffsetTime = OffsetTime

withHour :: Hour -> OffsetTime -> OffsetTime
withHour hour ot@OffsetTime {time} = ot {time = LocalTime.withHour hour time}

withMinute :: Minute -> OffsetTime -> OffsetTime
withMinute minute ot@OffsetTime {time} = ot {time = LocalTime.withMinute minute time}

withSecond :: Second -> OffsetTime -> OffsetTime
withSecond second ot@OffsetTime {time} = ot {time = LocalTime.withSecond second time}

withNanos :: Nano -> OffsetTime -> OffsetTime
withNanos nano ot@OffsetTime {time} = ot {time = LocalTime.withNano nano time}

plusHours :: Hour -> OffsetTime -> OffsetTime
plusHours hour ot@OffsetTime {time} = ot {time = LocalTime.plusHours (Hour.toInt hour) time}

plusMinutes :: Minute -> OffsetTime -> OffsetTime
plusMinutes minute ot@OffsetTime {time} = ot {time = LocalTime.plusMinutes (Minute.toInt minute) time}

plusSeconds :: Second -> OffsetTime -> OffsetTime
plusSeconds seconds ot@OffsetTime {time} = ot {time = LocalTime.plusSeconds (Second.toInt seconds) time}

plusNanos :: Nano -> OffsetTime -> OffsetTime
plusNanos nanos ot@OffsetTime {time} = ot {time = LocalTime.plusNanos (Nano.toInt nanos) time}

minusHours :: Hour -> OffsetTime -> OffsetTime
minusHours hour ot@OffsetTime {time} = ot {time = LocalTime.minusHours (Hour.toInt hour) time}

minusMinutes :: Minute -> OffsetTime -> OffsetTime
minusMinutes minute ot@OffsetTime {time} = ot {time = LocalTime.minusMinutes (Minute.toInt minute) time}

minusSeconds :: Second -> OffsetTime -> OffsetTime
minusSeconds seconds ot@OffsetTime {time} = ot {time = LocalTime.minusSeconds (Second.toInt seconds) time}

minusNanos :: Nano -> OffsetTime -> OffsetTime
minusNanos nanos ot@OffsetTime {time} = ot {time = LocalTime.minusNanos (Nano.toInt nanos) time}

toEpochNano :: OffsetTime -> Int64
toEpochNano OffsetTime {time, offset} =
  let nod = LocalTime.toNanoOfDay time
      offsetNanos = fromIntegral $ ZoneOffset.getTotalSeconds offset * 1_000_000_000
   in nod - offsetNanos

toEpochSecond:: LocalDate -> OffsetTime -> Int64
toEpochSecond ld OffsetTime {time, offset} =
  let epochDay = LocalDate.toEpochDay ld
      secs = epochDay * 86400 + LocalTime.toSecondOfDay time
   in secs - fromIntegral (ZoneOffset.getTotalSeconds offset)