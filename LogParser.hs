{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Attoparsec.Char8 hiding (space, take)
import Control.Monad (liftM)
import System.Environment (getArgs)
import Prelude hiding (takeWhile)
import Data.List (sortBy, foldl')
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (readInteger)
import System.Time.ParseDate (parseCalendarTime)
import System.Time
import System.Locale
import Debug.Trace

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
    which <- satisfy (\x -> x == 'i' || x == 'o')
    minutes <- takeTill (== 'x')
    _ <- x
    counter <- takeTill (== 'x')
    _ <- x
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
logDateEq a b = (getYear a) == (getYear b) &&
                (getMonth a) == (getMonth b) &&
                (getDay a) == (getDay b) &&
                (getHour a) == (getHour b) &&
                (getMinute a) == (getMinute b) &&
                (getSeconds a) == (getSeconds b)

-- 2012-08-22 18:32:08,505
{-parseDate :: String -> Maybe CalendarTime-}
{-parseDate dateStr = parseCalendarTime rfc822DateFormat "%Y-%m-%d %H:%M:%S" dateStr-}

countLines :: [L.ByteString] -> Integer
countLines = fromIntegral . length

maxConcurrent:: [L.ByteString] -> Integer
maxConcurrent = foldl' count' 0
    where
        count' acc l = case AL.maybeResult $ AL.parse line l of
            Just x  -> let conn = getConcurrentRequests $ getRequestId x
                       in if conn >= acc then conn else acc
            Nothing -> acc

protocolCount :: [L.ByteString] -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count M.empty
        where
            count acc l = case AL.maybeResult $ AL.parse line l of
                Just x -> M.insertWith (+) (S.copy (getProtocol x)) 1 acc
                Nothing -> acc

mapIfJust :: (a -> Maybe b) -> [a] -> [b]
mapIfJust f []      = []
mapIfJust f (x:xs)  = case f x of
                      Just x -> x : mapIfJust f xs
                      Nothing -> mapIfJust f xs

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a second level with the max num
plotDataConcurrentConn :: [L.ByteString] -> [(LogDate, Integer)]
plotDataConcurrentConn input = mapIfJust f input
        where
            f l = case AL.maybeResult $ AL.parse line l of
                Just x -> let conn = getConcurrentRequests $ getRequestId x
                              dateTime = getDate x
                          in Just (dateTime, conn)
                Nothing -> Nothing

plotDataConcurrentConn' :: [L.ByteString] -> [(LogDate, Integer)]
plotDataConcurrentConn' = reverse . foldl' f []
        where
            f acc l = case AL.maybeResult $ AL.parse line l of
                Just x -> let conn = getConcurrentRequests $ getRequestId x
                              dateTime = getDate x
                         in case (safehead acc) of
                            Just prev -> if logDateEq (fst prev) dateTime
                                    then (dateTime, max conn (snd prev)) : tail acc
                                    else (dateTime, conn) : acc
                            Nothing -> (dateTime, conn) : acc
                Nothing -> acc


showLines :: [L.ByteString] -> [Maybe LogLine]
showLines lines = take 5 $ map p_ lines
            where p_ x = AL.maybeResult $ AL.parse line x

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date) (getSeconds date)
pretty :: Show a => Integer -> (a, Integer) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

-- =================================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd, path] -> dispatch cmd path
    _ -> error "Invoke with <cmd> <path-to-log-file>"

dispatch :: Command -> FilePath -> IO ()
dispatch cmd = action
    where
        action = fromMaybe err (lookup cmd actions)
        err _  = putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

actions :: [(Command, FilePath -> IO ())]
actions = [("count", countLogFileLines)
          ,("show", showParsedLines)
          ,("maxConn", showMaxConcurrent)
          ,("plotConn", generatePlotDataConcurrentConn)
          ,("protocol", mapToTopList protocolCount)]

showParsedLines :: FilePath -> IO()
showParsedLines path = parseAndPrint path showLines

countLogFileLines :: FilePath -> IO ()
countLogFileLines path = parseAndPrint path countLines

showMaxConcurrent :: FilePath -> IO ()
showMaxConcurrent path = parseAndPrint path maxConcurrent

generatePlotDataConcurrentConn :: FilePath -> IO ()
generatePlotDataConcurrentConn path = do
        content <- L.readFile path
        let input = L.lines content
        let plotData = plotDataConcurrentConn' input
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ fst pd) (snd pd)) plotData

parseAndPrint :: (Show a) => FilePath -> ([L.ByteString] -> a) -> IO ()
parseAndPrint path f = print . f . L.lines =<< L.readFile path


mapToTopList :: ([L.ByteString] -> [(S.ByteString, Integer)]) -> FilePath -> IO ()
mapToTopList f p = do
    file <- liftM L.lines $ L.readFile p
    let mostPopular (_,a) (_,b) = compare b a
        m = f file
    mapM_ putStrLn . zipWith pretty [1..] . take 10 . sortBy mostPopular $ m
