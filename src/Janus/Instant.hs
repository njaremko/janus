module Janus.Instant
  ( Instant,
    now,
    ofEpochSecond,
    ofEpochMilli,
    parseIso8601,
    toYMD,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Attoparsec.Text as A
import Data.Bits ((.&.))
import Data.Char (isDigit, ord)
import Data.Int (Int64)
import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)
import Janus.Units
import qualified Janus.Units.Day as Day
import qualified Janus.Units.Month as Month
import qualified Janus.Units.Hour as Hour
import qualified Janus.Units.Minute as Minute
import qualified Janus.Units.Second as Second
import qualified Janus.Units.Year as Year
import qualified Janus.Units.Nano as Nano
import qualified Janus.Units.EpochSecond  as EpochSecond

import Prelude

-- An instantaneous point on the time-line.
data Instant = Instant
  { seconds :: EpochSecond,
    nanos :: Nano
  }
  deriving stock (Show, Eq, Ord, Bounded, Ix)

now :: IO Instant
now = do
  -- Current system time in UTC
  MkSystemTime seconds nanos <- getSystemTime
  return $
    Instant
      { seconds = EpochSecond.mkEpochSecond seconds,
        nanos = Nano.unsafeMkNano nanos
      }

ofEpochSecond :: EpochSecond -> Instant
ofEpochSecond seconds = Instant {seconds, nanos = Nano.unsafeMkNano @Int 0}

ofEpochMilli :: EpochSecond -> Instant
ofEpochMilli ms =
  let millis = EpochSecond.toInt @Int64 ms
      seconds = EpochSecond.mkEpochSecond $ millis `div` 1000
      mos = millis `mod` 1000
   in Instant {seconds, nanos = Nano.unsafeMkNano $ mos * 1_000_000}

-- >>> mkInstantWithOffset 2020 February 29 6 0 3 0 (-60)
-- Instant {seconds = 1582955943, nanos = 0}
mkInstantWithOffset :: Year -> Month -> Day -> Hour -> Minute -> Second -> Nano -> Int64 -> Instant
mkInstantWithOffset year month day hour minute seconds nanos offsetSeconds =
  let epochDays :: Int64 = epochDaysFromYMD year month day
      epochDaySeconds = secondsPerDay * epochDays
   in Instant
        { seconds = EpochSecond.mkEpochSecond $ epochDaySeconds + (secondsPerHour * Hour.toInt hour) + (secondsPerMinute * Minute.toInt minute) + Second.toInt seconds + offsetSeconds,
          nanos
        }

-- This is counter intuitive, but it's correct.
-- http://howardhinnant.github.io/date_algorithms.html
epochDaysFromYMD :: (Integral a) => Year -> Month -> Day -> a
epochDaysFromYMD y m d =
  let y1 = Year.toInt y
      month = Month.toOrdinal m
      day = Day.toInt d
      year = y1 - (if month <= 2 then 1 else 0)
      era =
        ( if 0 <= year
            then year
            else year - 399
        )
          `div` 400
      -- year of era
      yoe = (year - era * 400)
      -- day of year
      doy = (153 * (month + (if month > 2 then -3 else 9)) + 2) `div` 5 + day - 1
      -- day of era
      doe = yoe * 365 + yoe `div` 4 - yoe `div` 100 + doy
   in era * 146097 + doe - 719468

toYMD :: Instant -> (Year, Month, Day)
toYMD Instant {seconds} =
  let z = EpochSecond.toInt @Int64 seconds + 719468
      era = (if 0 <= z then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      y = yoe + era * 400
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      d = doy - (153 * mp + 2) `div` 5 + 1
      m = mp + (if mp < 10 then 3 else -9)
   in (fromIntegral (y + if m <= 2 then 1 else 0), Month.unsafeFromOrdinal m, fromIntegral d)

-- Parses timezone into offset seconds
timeZone :: Parser (Maybe Int64)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
        Just ':' -> anyChar *> twoDigits
        Just d | isDigit d -> twoDigits
        _ -> return 0
      let off
            | ch == '-' = negate off0
            | otherwise = off0
          off0 = fromIntegral (h * 60 + m) * secondsPerMinute
      case undefined of
        _
          | off == 0 ->
            return Nothing
          | off < -720 || off > 840 || m > 59 ->
            fail "invalid time zone offset"
          | otherwise -> return (Just off)

-- >>> maybeResult $ parse parseIso8601 "2021-06-07T19:36:40Z"
-- Just (Instant {seconds = 1623094600, nanos = 0})
parseIso8601 :: Parser Instant
parseIso8601 = do
  y <- (decimal @Int <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  m <- (twoDigits <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  d <- twoDigits <|> fail "date must be of form [+,-]YYYY-MM-DD"
  _ <- daySep
  h <- twoDigits
  minutes <- char ':' *> twoDigits
  (s, nanos) <- option (0, 0) (char ':' *> parseSeconds)
  offset <- timeZone
  case (mkYear y, Month.fromOrdinal m, mkDay d, mkHour h, mkMinute minutes, mkSecond s, mkNano nanos) of
    (Just year, Just month, Just day, Just hour, Just minute, Just second, Just ns) ->
      return $
        mkInstantWithOffset
          year
          month
          day
          hour
          minute
          second
          ns
          (fromMaybe 0 offset)
    _ -> fail "invalid time"

daySep :: Parser Char
daySep = satisfy (\c -> c == 'T' || c == ' ')

parseSeconds :: Parser (Int, Int)
parseSeconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit *> decimal
      return (real, t)
    _ -> return (real, 0)

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b
