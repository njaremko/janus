module Janus.Instant
  ( Instant,
    now,
    ofEpochSecond,
    ofEpochMilli,
    parseIso8601,
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
import Prelude
import qualified Janus.Units.Month as Month

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
      { seconds = EpochSecond seconds,
        nanos = fromIntegral nanos
      }

ofEpochSecond :: EpochSecond -> Instant
ofEpochSecond seconds = Instant {seconds, nanos = 0}

ofEpochMilli :: EpochSecond -> Instant
ofEpochMilli ms = let
    seconds = ms `div` 1000
    mos :: Nano = fromIntegral (ms `mod` 1000)
  in Instant {seconds, nanos = mos * 1_000_000}

-- >>> mkInstantWithOffset 2020 February 29 6 0 3 0 (-60)
-- Instant {seconds = 1582955943, nanos = 0}
mkInstantWithOffset :: Year -> Month -> Day -> Hour -> Minute -> Second -> Nano -> Int64 -> Instant
mkInstantWithOffset year month day hour minute seconds nanos offsetSeconds =
  let epochDays = epochDaysFromYMD (fromIntegral year) (Month.toOrdinal month) (fromIntegral day)
      epochDaySeconds = secondsPerDay * fromIntegral epochDays
   in Instant
        { seconds = EpochSecond $ epochDaySeconds + (secondsPerHour * fromIntegral hour) + (secondsPerMinute * fromIntegral minute) + fromIntegral seconds + offsetSeconds,
          nanos
        }

-- This is counter intuitive, but it's correct.
-- http://howardhinnant.github.io/date_algorithms.html
epochDaysFromYMD :: (Integral a) => a -> a -> a -> a
epochDaysFromYMD y month day =
  let year = y - (if month <= 2 then 1 else 0)
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
  minute <- char ':' *> twoDigits
  (s, ns) <- option (0, 0) (char ':' *> parseSeconds)
  offset <- timeZone
  case (mkYear y, Month.fromOrdinal m, mkDay d, mkHour h, mkMinute minute, mkSecond s, mkNano ns) of
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
