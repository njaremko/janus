module Janus.Instant
  ( Instant,
    now,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text as A
import Data.Bits ((.&.))
import Data.Char (isDigit, ord)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Janus.Units
import Prelude
import Data.Ix (Ix)

-- An instantaneous point on the time-line.
data Instant = Instant
  { seconds :: Int64,
    nanos :: Int
  } deriving stock (Show, Eq, Ord, Bounded, Ix)

now :: IO Instant
now = do
  MkSystemTime seconds nanos <- getSystemTime
  return $
    Instant
      { seconds,
        nanos = fromIntegral nanos
      }

-- >>> mkInstant 2020 2 29 6 0 3 0
-- Instant {seconds = 1582956003, nanos = 0}
mkInstant :: Year -> Month -> Day -> Hour -> Minute -> Second -> Nano -> Instant
mkInstant year month day hour minute seconds nanos =
  let epochDays = epochDaysFromYMD (fromIntegral year) (monthToInt month) (fromIntegral day)
      epochDaySeconds = secondsPerDay * fromIntegral epochDays
   in Instant
        { seconds = epochDaySeconds + (secondsPerHour * fromIntegral hour) + (secondsPerMinute * fromIntegral minute) + fromIntegral seconds,
          nanos = fromIntegral nanos
        }

-- This is counter intuitive, but it's correct.
-- http://howardhinnant.github.io/date_algorithms.html
epochDaysFromYMD :: (Integral a) => a -> a -> a -> a
epochDaysFromYMD y month day =
  let 
      year = y - (if month <= 2 then 1 else 0)
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

localTime :: Parser Instant
localTime = do
  y <- (decimal <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  m <- (twoDigits <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  d <- twoDigits <|> fail "date must be of form [+,-]YYYY-MM-DD"
  _ <- daySep
  h <- twoDigits
  m <- char ':' *> twoDigits
  (s, ns) <- option (0, 0) (char ':' *> parseSeconds)
  if h < 24 && m < 60 && s < 61
    then return $ Instant 0 0
    else fail "invalid time"

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
