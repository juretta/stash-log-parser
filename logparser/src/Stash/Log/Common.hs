{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stash.Log.Common
( logDateEqHour
, logDateEqMinute
, toLocalTime
) where

import Stash.Log.Parser
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Fixed

logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

logDateEqMinute :: LogDate -> LogDate -> Bool
logDateEqMinute a b = getYear a   == getYear b &&
                getMonth a        == getMonth b &&
                getDay a          == getDay b &&
                getHour a         == getHour b &&
                getMinute a       == getMinute b

-- | Convert LogDate to LocalTime
--
-- >>> toLocalTime (LogDate 2014 9 12 23 15 22 300)
-- 2014-09-12 23:15:22
toLocalTime :: LogDate -> LocalTime
toLocalTime LogDate{..} = LocalTime (fromGregorian (fromIntegral getYear) getMonth getDay) (TimeOfDay getHour getMinute (convertFixed getSeconds))
  where
    convertFixed :: Int -> Fixed E12
    convertFixed = (*10^12) . toEnum . fromEnum

