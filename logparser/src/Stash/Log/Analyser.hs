{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, Input
, ProtocolStats(..)
, DateValuePair(..)
, maxConcurrent
, plotDataConcurrentConnMinute
, plotDataConcurrentConnHour
, protocolStatsByHour
, showLines
) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (foldl', groupBy)
import Data.Function (on)
import Text.Printf (printf)
import Stash.Log.Parser
import Stash.Log.Common (logDateEqHour, logDateEqMin)

data DateValuePair = DateValuePair {
     getLogDate     :: !LogDate
    ,getValue       :: !Integer
} deriving (Show, Eq)


data ProtocolStats = ProtocolStats {
     getProtocolLogDate     :: !String
    ,getSsh                 :: !Int
    ,getHttp                :: !Int
}

-- | Count the number of lines in the given input file
countLines :: L.ByteString -> Integer
countLines input = toInteger $ L.count '\n' input

-- | Count the number of lines marked as incoming (aka request)
countRequestLines :: Input -> Integer
countRequestLines = countLinesWith (\x acc -> let rid = getRequestId x
                                              in if isIncoming rid then acc + 1 else acc)

maxConcurrent:: Input -> Integer
maxConcurrent = countLinesWith (\x acc -> let conn = getConcurrentRequests $ getRequestId x
                                          in if conn >= acc then conn else acc)

isSsh :: LogLine -> Bool
isSsh logLine = getProtocol logLine == "ssh"

isHttp :: LogLine -> Bool
isHttp logLine = proto == "http" || proto == "https"
                where proto = getProtocol logLine

-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a minute/hour level with the max num per unit
plotDataConcurrentConnMinute :: Input -> [DateValuePair]
plotDataConcurrentConnMinute = dataConcurrentConn logDateEqMin

plotDataConcurrentConnHour :: Input -> [DateValuePair]
plotDataConcurrentConnHour = dataConcurrentConn logDateEqHour

dataConcurrentConn :: (LogDate -> LogDate -> Bool) -> Input -> [DateValuePair]
dataConcurrentConn eqf inxs = reverse $ uncurry (++) res
        where
            f acc logLine =
                    let conn = getConcurrentRequests $ getRequestId logLine
                        dateTime = getDate logLine
                    in case acc of
                     ([prev], xs)-> dateTime `seq` if eqf (getLogDate prev) dateTime
                                 then ([DateValuePair dateTime (max conn (getValue prev))], xs)
                                 else ([DateValuePair dateTime conn], prev : xs)
                     ([], xs)    -> ([DateValuePair dateTime conn], xs)
                     (_, _)      -> ([], [])
            res = foldl' f ([],[]) $ parseLogLines inxs

protocolStatsByHour :: Input -> [ProtocolStats]
protocolStatsByHour rawLines = let  groups = groupBy (logDateEqHour `on` getDate) $ parseLogLines rawLines
                                    formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date) (getDay date) (getHour date)
                                in map (protocolStats formatLogDate) groups

protocolStats :: (LogDate -> String) -> [LogLine] -> ProtocolStats
protocolStats formatLogDate = foldl' aggregate (ProtocolStats "" 0 0)
                        where aggregate (ProtocolStats date ssh http) logLine =
                                    let !ssh'   = if isSsh logLine then ssh + 1 else ssh
                                        !http'  = if isHttp logLine then http + 1 else http
                                        !date'  = if null date then formatLogDate $ getDate logLine else date
                                    in ProtocolStats date' ssh' http'

countLinesWith :: (Num a) => (LogLine -> a -> a) -> Input -> a
countLinesWith f = foldl' count' 0
    where count' acc l = maybe acc (`f` acc) $ parseLogLine l

showLines :: Input -> [Maybe LogLine]
showLines = take 5 . map parseLogLine

