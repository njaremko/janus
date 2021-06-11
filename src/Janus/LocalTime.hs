module Janus.LocalTime
  ( LocalTime,
  )
where

import Data.Word (Word8)
import Prelude
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

-- A time without a time-zone in the ISO-8601 calendar system, such as 10:15:30.
data LocalTime = LocalTime
  { hour :: Word8,
    minute :: Word8,
    seconds :: Word8,
    nano :: Int
  }

now = do
    time <- getCurrentTime
    let x = iso8601Show time
    return 0