module Janus.LocalTime
  ( LocalTime,
    mkLocalTime,
    ofSecondOfDay,
    toSecondOfDay,
    ofNanoOfDay,
    toNanoOfDay,
    withHour,
    withMinute,
    withSecond,
    withNano,
    plusNanos,
    plusSeconds,
    plusMinutes,
    plusHours,
    minusSeconds,
    minusMinutes,
    minusHours,
    minusNanos,
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Janus.Units (Hour, Minute, Nano, Second, nanosPerHour, nanosPerMinute, nanosPerSecond)
import qualified Janus.Units.Hour as Hour
import qualified Janus.Units.Minute as Minute
import qualified Janus.Units.Nano as Nano
import qualified Janus.Units.Second as Second
import Prelude

-- A time without a time-zone in the ISO-8601 calendar system, such as 10:15:30.
data LocalTime = LocalTime
  { hour :: Hour,
    minute :: Minute,
    second :: Second,
    nano :: Nano
  }
  deriving stock (Show, Eq, Ord, Bounded, Ix)

mkLocalTime :: Hour -> Minute -> Second -> Nano -> LocalTime
mkLocalTime = LocalTime

withHour :: Hour -> LocalTime -> LocalTime
withHour hour d = d {hour}

withMinute :: Minute -> LocalTime -> LocalTime
withMinute minute d = d {minute}

withSecond :: Second -> LocalTime -> LocalTime
withSecond second d = d {second}

withNano :: Nano -> LocalTime -> LocalTime
withNano nano d = d {nano}

toSecondOfDay :: LocalTime -> Int64
toSecondOfDay LocalTime {hour, minute, second} =
  Hour.toInt hour * 3600 + Minute.toInt minute * 60 + Second.toInt second

ofSecondOfDay :: Int64 -> LocalTime
ofSecondOfDay secondOfDay =
  let hours = secondOfDay `div` 3600
      sod2 = secondOfDay - (hours * 3600)
      minutes = sod2 `div` 60
      sod3 = sod2 - (minutes * 60)
   in LocalTime
        (Hour.unsafeMkHour hours)
        (Minute.unsafeMkMinute minutes)
        (Second.unsafeMkSecond sod3)
        (Nano.unsafeMkNano @Int 0)

toNanoOfDay :: LocalTime -> Int64
toNanoOfDay LocalTime {hour, minute, second, nano} =
  (Hour.toInt hour * nanosPerHour)
    + (Minute.toInt minute * nanosPerMinute)
    + (Second.toInt second * nanosPerSecond)
    + Nano.toInt nano

ofNanoOfDay :: Int64 -> LocalTime
ofNanoOfDay nanoOfDay =
  let hours = nanoOfDay `div` 3600000000000
      nod2 = nanoOfDay - (hours * 3600000000000)
      minutes = nod2 `div` 60000000000
      nod3 = nod2 - (minutes * 60000000000)
      seconds = nod3 `div` 1000000000
      nod4 = nod3 - (seconds * 1000000000)
   in LocalTime
        (Hour.unsafeMkHour hours)
        (Minute.unsafeMkMinute minutes)
        (Second.unsafeMkSecond seconds)
        (Nano.unsafeMkNano nod4)

plusNanos :: Int64 -> LocalTime -> LocalTime
plusNanos 0 time = time
plusNanos nanosToAdd time =
  let nofd = toNanoOfDay time
      newNofd = (nanosToAdd `mod` 86400000000000 + nofd + 86400000000000) `mod` 86400000000000
   in if nofd == newNofd
        then time
        else
          let newHour = Hour.unsafeMkHour $ newNofd `div` 3600000000000
              newMinute = Minute.unsafeMkMinute $ newNofd `div` 60000000000 `mod` 60
              newSecond = Second.unsafeMkSecond $ newNofd `div` 1000000000 `mod` 60
              newNano = Nano.unsafeMkNano $ newNofd `mod` 1000000000
           in LocalTime
                newHour
                newMinute
                newSecond
                newNano

plusSeconds :: Int64 -> LocalTime -> LocalTime
plusSeconds 0 time = time
plusSeconds secondsToAdd time@LocalTime {hour, minute, second} =
  let sofd = Hour.toInt hour * 3600 + Minute.toInt minute * 60 + Second.toInt second
      newSofd = ((secondsToAdd `mod` 86400) + sofd + 86400) `mod` 86400
   in if sofd == newSofd
        then time
        else
          let newHour = Hour.unsafeMkHour $ newSofd `div` 3600
              newMinute = Minute.unsafeMkMinute $ newSofd `div` 60 `mod` 60
              newSecond = Second.unsafeMkSecond $ newSofd `mod` 60
           in time {hour = newHour, minute = newMinute, second = newSecond}

plusMinutes :: Int64 -> LocalTime -> LocalTime
plusMinutes 0 time = time
plusMinutes minutesToAdd time@LocalTime {hour, minute} =
  let mofd :: Int = Hour.toInt hour * 60 + Minute.toInt minute
      newMofd :: Int = fromIntegral ((minutesToAdd `mod` 1440) + fromIntegral mofd + 1440) `mod` 1440
   in if mofd == newMofd
        then time
        else
          let newHour = Hour.unsafeMkHour $ newMofd `div` 60
              newMinute = Minute.unsafeMkMinute $ newMofd `mod` 60
           in time {hour = newHour, minute = newMinute}

plusHours :: Int64 -> LocalTime -> LocalTime
plusHours 0 time = time
plusHours hoursToAdd time@LocalTime {hour} =
  let newHour =
        Hour.unsafeMkHour $
          ((hoursToAdd `mod` 24) + Hour.toInt hour + 24) `mod` 24
   in time {hour = newHour}

minusHours :: Int64 -> LocalTime -> LocalTime
minusHours hoursToSubtract = plusHours (- (hoursToSubtract `mod` 24))

minusMinutes :: Int64 -> LocalTime -> LocalTime
minusMinutes minutesToSubtract = plusMinutes (- (minutesToSubtract `mod` 1440))

minusSeconds :: Int64 -> LocalTime -> LocalTime
minusSeconds secondsToSubtract = plusSeconds (- (secondsToSubtract `mod` 86400))

minusNanos :: Int64 -> LocalTime -> LocalTime
minusNanos nanosToSubtract = plusNanos (- (nanosToSubtract `mod` 86400000000000))
