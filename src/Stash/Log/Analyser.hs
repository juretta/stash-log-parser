module Stash.Log.Analyser
( countLines
, maxConcurrent
, protocolCount
, plotDataConcurrentConnMinute
, plotDataConcurrentConnHour
, showLines
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Stash.Log.Parser

countLines :: [L.ByteString] -> Integer
countLines = fromIntegral . length

maxConcurrent:: [L.ByteString] -> Integer
maxConcurrent = foldl' count' 0
    where
        count' acc l = case parseLogLine l of
            Just logLine -> let conn = getConcurrentRequests $ getRequestId logLine
                            in if conn >= acc then conn else acc
            Nothing      -> acc

protocolCount :: [L.ByteString] -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count' M.empty
        where
            count' acc l = case parseLogLine l of
                Just logLine -> M.insertWith (+) (S.copy (getProtocol logLine)) 1 acc
                Nothing      -> acc

-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a minute/hour level with the max num per unit
plotDataConcurrentConnMinute :: [L.ByteString] -> [(LogDate, Integer)]
plotDataConcurrentConnMinute = dataConcurrentConn logDateEqMin

plotDataConcurrentConnHour :: [L.ByteString] -> [(LogDate, Integer)]
plotDataConcurrentConnHour = dataConcurrentConn logDateEqHour

dataConcurrentConn :: (LogDate -> LogDate -> Bool) -> [L.ByteString] -> [(LogDate, Integer)]
dataConcurrentConn eqf inxs = reverse $ (fst res) ++ (snd res)
        where 
            f acc l = case parseLogLine l of
                Just logLine    -> let conn = getConcurrentRequests $ getRequestId logLine
                                       dateTime = getDate logLine
                                   in case acc of
                                    ([], xs)    -> ([(dateTime, conn)], xs)
                                    ([prev], xs)-> if eqf (fst prev) dateTime
                                                then ([(dateTime, max conn (snd prev))], xs)
                                                else ([(dateTime, conn)], prev : xs)
                Nothing         -> acc
            res = foldl' f ([],[]) inxs

showLines :: [L.ByteString] -> [Maybe LogLine]
showLines lines_ = take 5 $ map parseLogLine lines_

