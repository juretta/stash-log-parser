module Stash.Log.Common
( logDateEqHour
, logDateEqMin
) where

import Stash.Log.Parser (LogDate(..))

-- | Do the given dates are within the same hour?
logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

logDateEqMin :: LogDate -> LogDate -> Bool
logDateEqMin a b = logDateEqHour a b &&
                getMinute a == getMinute b

