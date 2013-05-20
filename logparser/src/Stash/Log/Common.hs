{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Common
( logDateEqHour
, isSsh
, isHttp
) where

import Stash.Log.Parser

-- | Do the given dates are within the same hour?
logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

isSsh :: LogLine -> Bool
isSsh logLine = getProtocol logLine == "ssh"

isHttp :: LogLine -> Bool
isHttp logLine = proto == "http" || proto == "https"
                where proto = getProtocol logLine


