-- |
-- Copyright: (c) 2021 Nathan Jaremko
-- SPDX-License-Identifier: MIT
-- Maintainer: Nathan Jaremko <nathan@jaremko.ca>
--
-- See README for more info
module Janus
  ( someFunc,
  )
where

import Prelude
import Janus.Duration (Duration)
import qualified Janus.Duration as Duration

someFunc :: IO ()
someFunc = do
       let x1 = Duration.ofDays 1
       let x2 = Duration.ofDays 3
       putStrLn ("someFunc" :: String)
