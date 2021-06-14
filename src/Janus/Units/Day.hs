module Janus.Units.Day (Day, mkDay, unsafeMkDay, toInt) where

import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Prelude

newtype Day = Day Word8
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkDay :: (Integral a, Show a) => a -> Either Text Day
mkDay day = if 1 <= day && day <= 31 
  then Right (Day $ fromIntegral day) 
  else Left $ "Given value " <> T.pack (show day) <> " not a valid day of month"

unsafeMkDay :: (Integral a, Show a) => a -> Day
unsafeMkDay day = case mkDay day of
  Right a -> a
  Left err -> error $ T.unpack err

toInt :: (Integral a) => Day -> a
toInt (Day a) = fromIntegral a