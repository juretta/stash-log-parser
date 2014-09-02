{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Common
( logDateEqHour
) where

import Stash.Log.Parser

-- | Do the given dates are within the same hour?
logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

