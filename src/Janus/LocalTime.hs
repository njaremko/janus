module Janus.LocalTime
  ( LocalTime,
  )
where

import Data.Word (Word8)
import Prelude

-- A time without a time-zone in the ISO-8601 calendar system, such as 10:15:30.
data LocalTime = LocalTime
  { hour :: Word8,
    minute :: Word8,
    seconds :: Word8,
    nano :: Int
  }