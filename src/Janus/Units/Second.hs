module Janus.Units.Second
  ( Second,
    mkSecond,
    unsafeMkSecond,
    toInt,
  )
where

import Data.Ix (Ix)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

newtype Second = Second Int
  deriving newtype
    ( Show,
      Bounded,
      Ix,
      Enum,
      Num,
      Eq,
      Ord
    )

mkSecond :: (Integral a, Show a) => a -> Either Text Second
mkSecond second =
  if 0 <= second && second <= 59
    then Right (Second $ fromIntegral second)
    else Left $ "Given value " <> T.pack (show second) <> " isn't a valid second"

unsafeMkSecond :: (Integral a, Show a) => a -> Second
unsafeMkSecond second = case mkSecond second of
  Right a -> a
  Left err -> error $ T.unpack err

toInt :: (Integral a) => Second -> a
toInt (Second a) = fromIntegral a