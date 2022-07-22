module Janus.Units.Nano
  ( Nano,
    mkNano,
    unsafeMkNano,
    toInt,
  )
where

import Data.Int (Int64)
import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

newtype Nano = Nano Int64
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Eq,
      Ord,
      Num,
      Integral,
      Real
    )

mkNano :: (Integral a, Show a) => a -> Either Text Nano
mkNano nano =
  if 0 <= nano && nano <= 999_999_999
    then Right (Nano $ fromIntegral nano)
    else Left $ "Given value " <> T.pack (show nano) <> " isn't a valid nanosecond"

unsafeMkNano :: (Integral a, Show a) => a -> Nano
unsafeMkNano nano = case mkNano nano of
  Right a -> a
  Left err -> error $ T.unpack err

toInt :: (Integral a) => Nano -> a
toInt (Nano a) = fromIntegral a