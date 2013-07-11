{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, Input
, DateValuePair(..)
, concurrentConnections
, showLines
) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (foldl')
import Stash.Log.Parser
import Stash.Log.Common (logDateEqHour)

data DateValuePair = DateValuePair {
     getLogDate     :: !LogDate
    ,getValue       :: !Integer
} deriving (Show, Eq)




-- | Count the number of lines in the given input file
countLines :: L.ByteString -> Integer
countLines input = toInteger $ L.count '\n' input

-- | Count the number of lines marked as incoming (aka request)
countRequestLines :: Input -> Integer
countRequestLines = countLinesWith (\x acc -> if isIncoming $ getRequestId x then succ acc else acc)


-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a minute/hour level with the max num per unit
concurrentConnections :: Input -> [DateValuePair]
concurrentConnections inxs = reverse $ uncurry (++) res
        where
            eqf           = logDateEqHour
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



countLinesWith :: (Num a) => (LogLine -> a -> a) -> Input -> a
countLinesWith f = foldl' count' 0
    where count' acc l = maybe acc (`f` acc) $ parseLogLine l

showLines :: Input -> [Maybe LogLine]
showLines = take 5 . map parseLogLine

