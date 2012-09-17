{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, Input
, DateValuePair(..)
, maxConcurrent
, protocolCount
, plotDataConcurrentConnMinute
, plotDataConcurrentConnHour
, showLines
, countGitOperations
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

type Input = [L.ByteString]

countLines :: Input -> Integer
countLines = fromIntegral . length

countRequestLines :: Input -> Integer
countRequestLines = countLinesWith (\x acc ->   let rid = getRequestId x
                                                in if isIncoming rid then acc + 1 else acc)

maxConcurrent:: Input -> Integer
maxConcurrent = countLinesWith (\x acc ->   let conn = getConcurrentRequests $ getRequestId x
                                            in if conn >= acc then conn else acc)

countLinesWith :: (LogLine -> Integer -> Integer) -> Input -> Integer
countLinesWith p = foldl' count' 0
    where
        count' acc l = case parseLogLine l of
            Just logLine -> p logLine acc
            Nothing      -> acc

protocolCount :: Input -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count' M.empty
        where
            count' acc l = case parseLogLine l of
                Just logLine -> M.insertWith (+) (S.copy (getProtocol logLine)) 1 acc
                Nothing      -> acc

countGitOperations :: Input -> [(String,Int)]
countGitOperations inputLines = zip ["fetch", "clone", "push"] $ foldl' count' [0,0,0] inputLines
    where
        count' acc l = case parseLogLine l of
            Just logLine -> case acc of
                                [a,b,c] -> let  labels = getLabels logLine
                                                !a' = if elem "fetch" labels then a + 1 else a
                                                !b' = if (elem "shallow clone" labels || elem "clone" labels) then b + 1 else b
                                                !c' = if elem "push" labels then c + 1 else c
                                                in [a', b', c']
                                _       -> acc
            Nothing      -> acc

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
dataConcurrentConn eqf inxs = reverse $ (fst res) ++ (snd res)
        where
            f acc l = case parseLogLine l of
                Just logLine    -> let conn = getConcurrentRequests $ getRequestId logLine
                                       dateTime = getDate logLine
                                   in case acc of
                                    ([prev], xs)-> dateTime `seq` if eqf (getLogDate prev) dateTime
                                                then ([DateValuePair dateTime (max conn (getValue prev))], xs)
                                                else ([DateValuePair dateTime conn], prev : xs)
                                    ([], xs)    -> ([DateValuePair dateTime conn], xs)
                                    (_, _)      -> ([], [])
                Nothing         -> acc
            res = foldl' f ([],[]) inxs

showLines :: Input -> [Maybe LogLine]
showLines lines_ = take 5 $ map parseLogLine lines_


logDateEqMin :: LogDate -> LogDate -> Bool
logDateEqMin a b = logDateEqHour a b &&
                getMinute a == getMinute b

logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b
