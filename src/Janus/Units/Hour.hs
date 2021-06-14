module Janus.Units.Hour
  ( Hour,
    mkHour,
    unsafeMkHour,
    toInt,
  )
where

import Data.Ix (Ix)
import Data.Word (Word8)
import Prelude
import Data.Text (Text)
import qualified Data.Text as T

newtype Hour = Hour Word8
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkHour :: (Integral a, Show a) => a -> Either Text Hour
mkHour hour = if 0 <= hour && hour <= 23 
  then Right (Hour $ fromIntegral hour) 
  else Left $ "Given value " <> T.pack (show hour) <> " isn't a valid hour"

unsafeMkHour :: (Integral a, Show a) => a -> Hour
unsafeMkHour hour = case mkHour hour of
  Right a -> a
  Left err -> error $ T.unpack err

toInt :: (Integral a) => Hour -> a
toInt (Hour a) = fromIntegral a