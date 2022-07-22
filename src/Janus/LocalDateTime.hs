module Janus.LocalDateTime
  ( LocalDateTime,
    mkLocalDateTime,
    ofEpochSecond,
    plusYears,
    getYear,
    getMonth,
    getDayOfMonth,
    getHour,
    getMinute,
    getSecond,
    getNano,
    now,
    fromUtcTime,
    fromSystemTime,
  )
where

import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Ix (Ix)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.System (SystemTime (..))
import qualified Data.Time.Clock.System as SystemTime
import Janus.LocalDate (LocalDate)
import qualified Janus.LocalDate as LocalDate
import Janus.LocalTime (LocalTime)
import qualified Janus.LocalTime as LocalTime
import Janus.Units (Day, EpochSecond, Hour, Minute, Month, Nano, Second, Unit (EpochSecond), Year)
import qualified Janus.Units.ChronoField as ChronoField
import qualified Janus.Units.EpochDay as EpochDay
import qualified Janus.Units.EpochSecond as EpochSecond
import Janus.ZoneOffset (ZoneOffset)
import qualified Janus.ZoneOffset as ZoneOffset
import Prelude

-- A date-time without a time-zone in the ISO-8601 calendar system, such as 2007-12-03T10:15:30.
data LocalDateTime = LocalDateTime
  { date :: LocalDate,
    time :: LocalTime
  }
  deriving stock (Show, Eq, Ord, Bounded, Ix)

mkLocalDateTime :: LocalDate -> LocalTime -> LocalDateTime
mkLocalDateTime = LocalDateTime

fromUtcTime :: UTCTime -> LocalDateTime
fromUtcTime = fromSystemTime . SystemTime.utcToSystemTime

fromSystemTime :: SystemTime -> LocalDateTime
fromSystemTime MkSystemTime {systemSeconds, systemNanoseconds} = do
  fromRight (error "Failed to convert system time to local date time") $
    ofEpochSecond (EpochSecond.mkEpochSecond systemSeconds) (fromIntegral systemNanoseconds) ZoneOffset.utcZoneOffset

now :: IO LocalDateTime
now = fromSystemTime <$> SystemTime.getSystemTime

ofEpochSecond :: EpochSecond -> Nano -> ZoneOffset -> Either Text LocalDateTime
ofEpochSecond epochSecond nanoOfSecond offset = do
  _ <- ChronoField.checkValid ChronoField.NanoOfSecond nanoOfSecond
  let localSecond = epochSecond + fromIntegral (ZoneOffset.getTotalSeconds offset)
      localEpochDay = EpochDay.mkEpochDay $ localSecond `div` 86400
      secsOfDay :: Int64 = EpochSecond.toInt $ localSecond `mod` 86400
      date = LocalDate.fromEpochDay localEpochDay
      time = LocalTime.fromNanoOfDay (secsOfDay * 1000000000 + fromIntegral nanoOfSecond)
  Right $ mkLocalDateTime date time

getYear :: LocalDateTime -> Year
getYear LocalDateTime {date} = LocalDate.getYear date

getMonth :: LocalDateTime -> Month
getMonth LocalDateTime {date} = LocalDate.getMonth date

getDayOfMonth :: LocalDateTime -> Day
getDayOfMonth LocalDateTime {date} = LocalDate.getDayOfMonth date

getHour :: LocalDateTime -> Hour
getHour LocalDateTime {time} = LocalTime.getHour time

getMinute :: LocalDateTime -> Minute
getMinute LocalDateTime {time} = LocalTime.getMinute time

getSecond :: LocalDateTime -> Second
getSecond LocalDateTime {time} = LocalTime.getSecond time

getNano :: LocalDateTime -> Nano
getNano LocalDateTime {time} = LocalTime.getNano time

plusYears :: Int -> LocalDateTime -> LocalDateTime
plusYears years ldt@LocalDateTime {date} = ldt {date = LocalDate.plusYears years date}