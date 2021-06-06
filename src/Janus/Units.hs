module Janus.Units where

import Prelude
import Data.Int (Int64)

secondsPerMinute :: Int64
secondsPerMinute = 60

secondsPerHour :: Int64
secondsPerHour = secondsPerMinute * minutesPerHour

secondsPerDay :: Int64
secondsPerDay = secondsPerHour * hoursPerDay

hoursPerDay :: Int64
hoursPerDay = 24

minutesPerHour :: Int64
minutesPerHour = 60

nanosPerSecond :: Int64
nanosPerSecond = 1000_000_000

nanosPerMinute :: Int64
nanosPerMinute = nanosPerSecond * secondsPerMinute

nanosPerHour :: Int64
nanosPerHour = nanosPerMinute * minutesPerHour

nanosPerDay :: Int64
nanosPerDay = nanosPerHour * hoursPerDay