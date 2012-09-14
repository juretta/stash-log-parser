{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, DateValuePair(..)
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

data DateValuePair = DateValuePair {
     getLogDate     :: !LogDate
    ,getValue       :: !Integer
} deriving (Show, Eq)

countLines :: [L.ByteString] -> Integer
countLines = fromIntegral . length

countRequestLines :: [L.ByteString] -> Integer
countRequestLines = countLinesWith (\x acc ->   let rid = getRequestId x
                                                in if isIncoming rid then acc + 1 else acc)

maxConcurrent:: [L.ByteString] -> Integer
maxConcurrent = countLinesWith (\x acc ->   let conn = getConcurrentRequests $ getRequestId x
                                            in if conn >= acc then conn else acc)

countLinesWith :: (LogLine -> Integer -> Integer) -> [L.ByteString] -> Integer
countLinesWith p = foldl' count' 0
    where
        count' acc l = case parseLogLine l of
            Just logLine -> p logLine acc
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
plotDataConcurrentConnMinute :: [L.ByteString] -> [DateValuePair]
plotDataConcurrentConnMinute = dataConcurrentConn logDateEqMin

plotDataConcurrentConnHour :: [L.ByteString] -> [DateValuePair]
plotDataConcurrentConnHour = dataConcurrentConn logDateEqHour



dataConcurrentConn :: (LogDate -> LogDate -> Bool) -> [L.ByteString] -> [DateValuePair]
dataConcurrentConn eqf inxs = reverse $ (fst res) ++ (snd res)
        where
            f acc l = case parseLogLine l of
                Just logLine    -> let conn = getConcurrentRequests $ getRequestId logLine
                                       dateTime = getDate logLine
                                   in case acc of
                                    ([], xs)    -> ([DateValuePair dateTime conn], xs)
                                    ([prev], xs)-> dateTime `seq` if eqf (getLogDate prev) dateTime
                                                then ([DateValuePair dateTime (max conn (getValue prev))], xs)
                                                else ([DateValuePair dateTime conn], prev : xs)
                Nothing         -> acc
            res = foldl' f ([],[]) inxs

showLines :: [L.ByteString] -> [Maybe LogLine]
showLines lines_ = take 5 $ map parseLogLine lines_

