module Janus.Units.Minute
  ( Minute,
    mkMinute,
    unsafeMkMinute,
    toInt,
  )
where

import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

newtype Minute = Minute Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkMinute :: (Integral a, Show a) => a -> Either Text Minute
mkMinute minute =
  if 0 <= minute && minute <= 59
    then Right (Minute $ fromIntegral minute)
    else Left $ "Given value " <> T.pack (show minute) <> " isn't a valid minute"

unsafeMkMinute :: (Integral a, Show a) => a -> Minute
unsafeMkMinute minute = case mkMinute minute of
  Right a -> a
  Left err -> error $ T.unpack err

toInt :: (Integral a) => Minute -> a
toInt (Minute a) = fromIntegral a