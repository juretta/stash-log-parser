{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, Input
, GitOperationStats(..)
, ProtocolStats(..)
, DateValuePair(..)
, maxConcurrent
, protocolCount
, plotDataConcurrentConnMinute
, plotDataConcurrentConnHour
, plotGitOperations
, protocolStatsByHour
, showLines
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import Data.List (foldl', groupBy)
import Data.Maybe (mapMaybe, isJust)
import Data.Function (on)
import Text.Printf (printf)
import Stash.Log.Parser

data DateValuePair = DateValuePair {
     getLogDate     :: !LogDate
    ,getValue       :: !Integer
} deriving (Show, Eq)

type Input = [L.ByteString]

data ProtocolStats = ProtocolStats {
     getProtocolLogDate     :: !String
    ,getSsh                 :: !Int
    ,getHttp                :: !Int
}

data GitOperationStats = GitOperationStats {
     getOpStatDate              :: String
    ,cacheMisses                :: [Int] -- clone, fetch, shallow clone, push, ref advertisement
    ,cacheHits                  :: [Int]
}

emptyStats :: GitOperationStats
emptyStats = GitOperationStats "" zero zero
        where zero = replicate 5 0

-- | Count the number of lines in the given input file
countLines :: L.ByteString -> Integer
countLines input = toInteger $ L.count '\n' input

countRequestLines :: Input -> Integer
countRequestLines = countLinesWith (\x acc -> let rid = getRequestId x
                                              in if isIncoming rid then acc + 1 else acc)

maxConcurrent:: Input -> Integer
maxConcurrent = countLinesWith (\x acc -> let conn = getConcurrentRequests $ getRequestId x
                                          in if conn >= acc then conn else acc)

countLinesWith :: (Num a) => (LogLine -> a -> a) -> Input -> a
countLinesWith f = foldl' count' 0
    where count' acc l = maybe acc (`f` acc) $ parseLogLine l

protocolCount :: Input -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count' M.empty . mapMaybe parseLogLine
        where
            count' acc logLine = let !proto = getProtocol logLine
                                 in M.insertWith (+) proto 1 acc

inLabel :: LogLine -> String -> Bool
inLabel logLine name =  let labels = getLabels logLine
                        in name `elem` labels


-- As of 1.1.2 of the clone cache plugin, refs are explicitly listed in the
-- labels field, most of the data we have does _not_ have that information though
isRefAdvertisement :: LogLine -> Bool
isRefAdvertisement logLine = ".git/info/refs" `S.isSuffixOf` path && "GET" == method && isJust username
            where
                action      = getAction logLine
                path        = getPath action
                method      = getMethod action
                username    = getUsername logLine

isCacheHit :: LogLine -> Bool
isCacheHit logLine = inLabel logLine "cache:hit"

isCacheMiss :: LogLine -> Bool
isCacheMiss = not . flip inLabel "cache:hit" -- treat as cache miss if the cache:* label is missing

isFetch :: LogLine -> Bool
isFetch logLine = inLabel logLine "fetch" && not (inLabel logLine "clone" || inLabel logLine "shallow clone")

isClone :: LogLine -> Bool
isClone logLine = inLabel logLine "clone"

isShallowClone :: LogLine -> Bool
isShallowClone logLine = inLabel logLine "shallow clone"

isPush :: LogLine -> Bool
isPush logLine = inLabel logLine "push"

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
            res = foldl' f ([],[]) $ parseLines inxs

parseLines :: Input -> [LogLine]
parseLines = mapMaybe parseLogLine

-- Example Output
-- (Date, clone, fetch, shallow clone, push)
plotGitOperations :: Input -> [GitOperationStats]
plotGitOperations rawLines =
    let formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date)
    in plotGitOperations' logDateEqHour formatLogDate rawLines

plotGitOperations' :: (LogDate -> LogDate -> Bool) -> (LogDate -> String) -> Input -> [GitOperationStats]
plotGitOperations' comp formatLogDate rawLines =
    let groups = groupBy (comp `on` getDate) $ parseLines rawLines
    in map (summarizeGitOperations formatLogDate) groups


cachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
cachedOperation op logLine = op logLine && isCacheHit logLine

uncachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
uncachedOperation op logLine = op logLine && isCacheMiss logLine

updateStats :: (LogDate -> String) -> GitOperationStats -> LogLine -> GitOperationStats
updateStats formatLogDate (GitOperationStats date misses hits) logLine =
                            let ops         = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
                                inc op      = if op logLine then (+1) else (+0)
                                missOps     = map (inc . uncachedOperation) ops
                                hitOps      = map (inc . cachedOperation) ops
                                !date'      = if null date then formatLogDate $ getDate logLine else date
                                !misses'    = zipWith id missOps misses
                                !hits'      = zipWith id hitOps hits
                            in GitOperationStats date' misses' hits'

summarizeGitOperations :: (LogDate -> String) -> [LogLine] -> GitOperationStats
summarizeGitOperations formatLogDate = foldl' aggregate emptyStats . filter isOutgoingLogLine
                        where aggregate = updateStats formatLogDate

protocolStatsByHour :: Input -> [ProtocolStats]
protocolStatsByHour rawLines = let  groups = groupBy (logDateEqHour `on` getDate) $ parseLines rawLines
                                    formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date) (getDay date) (getHour date)
                                in map (protocolStats formatLogDate) groups

protocolStats :: (LogDate -> String) -> [LogLine] -> ProtocolStats
protocolStats formatLogDate = foldl' aggregate (ProtocolStats "" 0 0)
                        where aggregate (ProtocolStats date ssh http) logLine =
                                    let !ssh'   = if isSsh logLine then ssh + 1 else ssh
                                        !http'  = if isHttp logLine then http + 1 else http
                                        !date'  = if null date then formatLogDate $ getDate logLine else date
                                    in ProtocolStats date' ssh' http'

showLines :: Input -> [Maybe LogLine]
showLines = take 5 . map parseLogLine

logDateEqMin :: LogDate -> LogDate -> Bool
logDateEqMin a b = logDateEqHour a b &&
                getMinute a == getMinute b

logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b