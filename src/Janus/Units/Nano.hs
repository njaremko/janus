module Janus.Units.Nano
  ( Nano,
    mkNano,
    unsafeMkNano,
    toInt,
  )
where

import Data.Ix (Ix)
import Prelude
import Data.Maybe (fromMaybe)

newtype Nano = Nano Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord
    )

mkNano :: (Integral a) => a -> Maybe Nano
mkNano nano = if 0 <= nano && nano <= 999_999_999 then Just (Nano $ fromIntegral nano) else Nothing

unsafeMkNano :: (Integral a) => a -> Nano
unsafeMkNano nano = fromMaybe (error "") $ mkNano nano

toInt :: (Integral a) => Nano -> a
toInt (Nano a) = fromIntegral a