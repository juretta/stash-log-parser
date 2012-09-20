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
, plotGitOperations
, showLines
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import Data.List (foldl', groupBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Text.Printf (printf)
import Stash.Log.Parser

data DateValuePair = DateValuePair {
     getLogDate     :: !LogDate
    ,getValue       :: !Integer
} deriving (Show, Eq)

type Input = [L.ByteString]

countLines :: Input -> Integer
countLines = fromIntegral . length

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
protocolCount logLines = M.toList . foldl' count' M.empty $ mapMaybe parseLogLine logLines
        where
            count' acc logLine = M.insertWith (+) (S.copy (getProtocol logLine)) 1 acc

inLabel :: LogLine -> String -> Bool
inLabel logLine name =  let labels = getLabels logLine
                        in name `elem` labels

isFetch :: LogLine -> Bool
isFetch logLine = inLabel logLine "fetch" && not (inLabel logLine "clone" || inLabel logLine "shallow clone")

isClone :: LogLine -> Bool
isClone logLine = inLabel logLine "clone"

isShallowClone :: LogLine -> Bool
isShallowClone logLine = inLabel logLine "shallow clone"

isPush :: LogLine -> Bool
isPush logLine = inLabel logLine "push"

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

parseLines :: Input -> [LogLine]
parseLines = mapMaybe parseLogLine

-- Example Output
-- (Date, clone, fetch, shallow clone, push)
plotGitOperations :: (Num a) => Input -> [(String, a, a, a, a)]
plotGitOperations rawLines =
    let groups = groupBy (logDateEqHour `on` getDate) $ parseLines rawLines
        formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date)
        aggregate = foldl' (\acc logLine -> case acc of
                                                (date, clone, fetch, shallowClone, push)
                                                    -> let !clone'          = if isClone logLine then clone + 1 else clone
                                                           !fetch'          = if isFetch logLine then fetch + 1 else fetch
                                                           !shallowClone'   = if isShallowClone logLine then shallowClone + 1 else shallowClone
                                                           !push'           = if isPush logLine then push + 1 else push
                                                           !date'           = if null date then formatLogDate $ getDate logLine else date
                                                           in (date', clone', fetch', shallowClone', push')
                            ) ("", 0,0,0,0)
    in map aggregate groups



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
