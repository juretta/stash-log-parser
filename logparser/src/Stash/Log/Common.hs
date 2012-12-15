{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Common
( logDateEqHour
, logDateEqMin
, sortLogFiles
, isSsh
, isHttp
) where

import Stash.Log.Parser
import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.String.Utils (split)
import System.Path.NameManip

-- | Do the given dates are within the same hour?
logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

logDateEqMin :: LogDate -> LogDate -> Bool
logDateEqMin a b = logDateEqHour a b &&
                getMinute a == getMinute b

isSsh :: LogLine -> Bool
isSsh logLine = getProtocol logLine == "ssh"

isHttp :: LogLine -> Bool
isHttp logLine = proto == "http" || proto == "https"
                where proto = getProtocol logLine


-- | Sort the logfiles by date and log file sequence number
-- The logfile naming scheme is: "atlassian-stash-access-2012-11-29.0.log(.bz2)"
sortLogFiles :: [FilePath] -> [FilePath]
sortLogFiles = sortBy logFilePred
    where extractFile = last . slice_path
          sortPred (date1, num1) (date2, num2) = compare date1 date2 `mappend` compare num1 num2
          logFilePred logFileName1 logFileName2 = sortPred (extractSortPairs logFileName1) (extractSortPairs logFileName2)
          extractSortPairs path = let elems = drop 3 $ split "-" $ extractFile path
                                  in case elems of
                                     (year:month:(rest:_)) -> case split "." rest of
                                                             (day:num:_) -> (year ++ "-" ++ month ++ "-" ++ day, read num :: Int)
                                                             _           -> ("", 0)
                                     _                 -> ("9999", 0)
