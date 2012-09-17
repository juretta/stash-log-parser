{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Parser
( InOurOut
, RequestId(..)
, LogLine(..)
, LogDate(..)
, parseLogLine
, isIncoming
, isOutgoing
) where

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Attoparsec.Char8 hiding (char, space, take)
import Prelude hiding (takeWhile)
import Data.ByteString.Char8 (readInteger)
import Data.String.Utils (split)

-- REMOTE_ADRESS | PROTOCOL | (o|i)REQUEST_ID | USERNAME | date |  URL | DETAILS | LABELS | TIME | SESSION_ID |
-- REQUEST_ID -> MINUTE_OF_DAYxREQUEST_COUNTERxCONCURRENT_REQUESTS

data InOurOut = In | Out deriving (Show, Eq)

data RequestId = RequestId {
     getInOrOut             :: !Char
    ,getRequestCounter      :: !Integer
    ,getConcurrentRequests  :: !Integer
} deriving (Show, Eq)

isIncoming :: RequestId -> Bool
isIncoming rid = getInOrOut rid == 'i'

isOutgoing :: RequestId -> Bool
isOutgoing rid = getInOrOut rid == 'o'

data LogLine = LogLine {
     getRemoteAdress        :: S.ByteString
    ,getProtocol            :: S.ByteString
    ,getRequestId           :: RequestId
    ,getUsername            :: S.ByteString
    ,getDate                :: LogDate
    ,getAction              :: S.ByteString
    ,getDetails             :: S.ByteString
    ,getLabels              :: [String]
    ,getRequestDuration     :: S.ByteString
    ,getSessionId           :: S.ByteString
} deriving (Show, Eq)

data LogDate = LogDate {
    getYear         :: !Int
    ,getMonth       :: !Int
    ,getDay         :: !Int
    ,getHour        :: !Int
    ,getMinute      :: !Int
    ,getSeconds     :: !Int
    ,getMillis      :: !Int
} deriving (Show, Eq)

parseLogLine :: L.ByteString -> Maybe LogLine
parseLogLine line = AL.maybeResult $ AL.parse parseLine line

-- =================================================================================

pipe, space, dash, colon, comma, x :: Parser Char
pipe        = satisfy (== '|')
space       = satisfy (== ' ')
dash        = satisfy (== '-')
colon       = satisfy (== ':')
comma       = satisfy (== ',')
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
    space
    pipe
    space
    return $ LogDate year month day hour minute second millis

logEntry :: Parser S.ByteString
logEntry = do
   entry <- takeTill (== '|')
   pipe
   space
   return $ S.init entry

-- | Parse the request Id. A request id consist of a a char indicating an
-- request 'i' or response 'o', followed by the minute of the day, a request
-- counter and the number of concurrent requests, separated by an 'x'. E.g. i8x1401519x6
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

-- | Parse an access log line
parseLine :: Parser LogLine
parseLine = do
    remoteAddress <- logEntry
    protocol <- logEntry
    requestId <- parseRequestId
    username <- logEntry
    date <- parseLogEntryDate
    action <- logEntry
    details <- logEntry
    labels_ <- logEntry
    duration <- logEntry
    sessionId <- logEntry
    let labels = split "," (S.unpack labels_)
    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

{-
Example log line
- 63.246.22.198,172.16.3.45 | https | o16x1402216x7 | cbac-confluence-user | 2012-09-08 00:17:00,270 | "GET /scm/ATLASSIAN/confluence.git/info/refs HTTP/1.1" | "" "JGit/unknown" | - | 1506 | - |
-}
