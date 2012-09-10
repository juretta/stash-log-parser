{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Parser where

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Attoparsec.Char8 hiding (char, space, take)
import Prelude hiding (takeWhile)
import Data.List (foldl')
import Text.Printf (printf)
import Data.ByteString.Char8 (readInteger)

type Command = String

-- REMOTE_ADRESS | PROTOCOL | (o|i)REQUEST_ID | USERNAME | date   URL | DETAILS | LABELS | TIME | SESSION_ID |
-- REQUEST_ID -> MINUTE_OF_DAYxREQUEST_COUNTERxCONCURRENT_REQUESTS

data InOurOut = In | Out deriving (Show, Eq)

data RequestId = RequestId {
     getInOrOut             :: Char
    ,getRequestCounter      :: Integer
    ,getConcurrentRequests  :: Integer
} deriving (Show, Eq)

data LogLine = LogLine {
     getRemoteAdress        :: S.ByteString
    ,getProtocol            :: S.ByteString
    ,getRequestId           :: RequestId
    ,getUsername            :: S.ByteString
    ,getDate                :: LogDate
    ,getAction              :: S.ByteString
    ,getDetails             :: S.ByteString
    ,getLabels              :: S.ByteString
    ,getRequestDuration     :: S.ByteString
    ,getSessionId           :: S.ByteString
} deriving (Show, Eq)

data LogDate = LogDate {
    getYear :: Int
    ,getMonth :: Int
    ,getDay :: Int
    ,getHour :: Int
    ,getMinute :: Int
    ,getSeconds :: Int
    ,getMillis :: Int
} deriving (Show, Eq)

pipe, space, dash, colon, comma, quote, x :: Parser Char
pipe        = satisfy (== '|')
space       = satisfy (== ' ')
dash        = satisfy (== '-')
colon       = satisfy (== ':')
comma       = satisfy (== ',')
quote       = satisfy (== '\"')
x           = satisfy (== 'x')

-- 2012-08-22 18:32:08,505
parseLogEntryDate :: Parser LogDate
parseLogEntryDate = do
    year <- decimal
    dash
    month <- decimal
    dash
    day <- decimal
    space
    hour <- decimal
    colon
    minute <- decimal
    colon
    second <- decimal
    comma
    millis <- decimal
    return $ LogDate year month day hour minute second millis

logEntry :: Parser S.ByteString
logEntry = do
   entry <- takeTill (== '|')
   pipe
   space
   return $ S.init entry

-- | i8x1401519x6 |
parseRequestId :: Parser RequestId
parseRequestId = do
    which <- satisfy (\c -> c == 'i' || c == 'o')
    _ <- takeTill (== 'x')
    x
    counter <- takeTill (== 'x')
    x
    concurrent <- takeTill (== ' ')
    space
    pipe
    space
    return $ RequestId which (maybe 0 fst $ readInteger counter) (maybe 0 fst $ readInteger concurrent)

{-

- 63.246.22.198,172.16.3.45 | https | o16x1402216x7 | cbac-confluence-user | 2012-09-08 00:17:00,270 | "GET /scm/ATLASSIAN/confluence.git/info/refs HTTP/1.1" | "" "JGit/unknown" | - | 1506 | - |

-}
line :: Parser LogLine
line = do
    remoteAddress <- logEntry
    protocol <- logEntry
    requestId <- parseRequestId
    username <- logEntry
    date <- parseLogEntryDate
    action <- logEntry
    details <- logEntry
    labels <- logEntry
    duration <- logEntry
    sessionId <- logEntry

    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

-- Two LogDates are considered equal if every field apart from the millis field
-- matches
logDateEq :: LogDate -> LogDate -> Bool
logDateEq a b = getYear a == (getYear b) &&
                getMonth a == (getMonth b) &&
                getDay a == (getDay b) &&
                getHour a == (getHour b) &&
                getMinute a == (getMinute b) &&
                getSeconds a == (getSeconds b)

logDateEqMin :: LogDate -> LogDate -> Bool
logDateEqMin a b = getYear a == (getYear b) &&
                getMonth a == (getMonth b) &&
                getDay a == (getDay b) &&
                getHour a == (getHour b) &&
                getMinute a == (getMinute b)

logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a == (getYear b) &&
                getMonth a == (getMonth b) &&
                getDay a == (getDay b) &&
                getHour a == (getHour b)

-- 2012-08-22 18:32:08,505
{-parseDate :: String -> Maybe CalendarTime-}
{-parseDate dateStr = parseCalendarTime rfc822DateFormat "%Y-%m-%d %H:%M:%S" dateStr-}

countLines :: [L.ByteString] -> Integer
countLines = fromIntegral . length

maxConcurrent:: [L.ByteString] -> Integer
maxConcurrent = foldl' count' 0
    where
        count' acc l = case AL.maybeResult $ AL.parse line l of
            Just logLine -> let conn = getConcurrentRequests $ getRequestId logLine
                       in if conn >= acc then conn else acc
            Nothing      -> acc

protocolCount :: [L.ByteString] -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count' M.empty
        where
            count' acc l = case AL.maybeResult $ AL.parse line l of
                Just logLine -> M.insertWith (+) (S.copy (getProtocol logLine)) 1 acc
                Nothing      -> acc

-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a second level with the max num
plotDataConcurrentConn' :: [L.ByteString] -> [(LogDate, Integer)]
plotDataConcurrentConn' inxs = reverse $ snd $ foldl' f ([],[]) inxs
        where
            f acc l = case AL.maybeResult $ AL.parse line l of
                Just logLine    -> let conn = getConcurrentRequests $ getRequestId logLine
                                       dateTime = getDate logLine
                                   in case acc of
                                    ([], xs)    -> ([(dateTime, conn)], xs)
                                    ([prev], xs)-> if logDateEqHour (fst prev) dateTime
                                                then ([(dateTime, max conn (snd prev))], xs)
                                                else ([(dateTime, conn)], prev : xs)
                Nothing         -> acc


showLines :: [L.ByteString] -> [Maybe LogLine]
showLines lines_ = take 5 $ map p_ lines_
            where p_ l = AL.maybeResult $ AL.parse line l

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date) (getSeconds date)
pretty :: Show a => Integer -> (a, Integer) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

