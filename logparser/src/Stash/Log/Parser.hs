{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Parser
( Action(..)
, InOurOut
, RequestId(..)
, LogLine(..)
, LogDate(..)
, Input
, parseLogLine
, parseLines
, isIncoming
, isOutgoing
, isOutgoingLogLine
) where

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Attoparsec.Char8 hiding (char, space, take)
import Prelude hiding (takeWhile)
import Data.ByteString.Char8 (readInteger)
import Data.String.Utils (split)
import Data.Maybe (mapMaybe)
-- REMOTE_ADRESS | PROTOCOL | (o|i)REQUEST_ID | USERNAME | date |  URL | DETAILS | LABELS | TIME | SESSION_ID |
-- REQUEST_ID -> MINUTE_OF_DAYxREQUEST_COUNTERxCONCURRENT_REQUESTS

type Input = [L.ByteString]

data Action = Action {
     getMethod       :: S.ByteString
    ,getPath         :: S.ByteString
} deriving (Show, Eq)

data InOurOut = In | Out deriving (Show, Eq)

data RequestId = RequestId {
     getInOrOut             :: !Char
    ,getRequestCounter      :: !Integer
    ,getConcurrentRequests  :: !Integer
} deriving (Show, Eq)

data LogLine = LogLine {
     getRemoteAdress        :: S.ByteString
    ,getProtocol            :: S.ByteString
    ,getRequestId           :: RequestId
    ,getUsername            :: Maybe S.ByteString
    ,getDate                :: LogDate
    ,getAction              :: Action
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

-- | Parse the input into a list of LogLines
parseLines :: Input -> [LogLine]
parseLines = mapMaybe parseLogLine

-- | Parse a single log line.
parseLogLine :: L.ByteString -> Maybe LogLine
parseLogLine line = AL.maybeResult $ AL.parse parseLine line

-- | Check whether this is a log line for the request ("incoming")
isIncoming :: RequestId -> Bool
isIncoming rid = getInOrOut rid == 'i'

-- | Check whether this is a log line for a response ("outgoing")
isOutgoing :: RequestId -> Bool
isOutgoing rid = getInOrOut rid == 'o'

isOutgoingLogLine :: LogLine -> Bool
isOutgoingLogLine = isOutgoing . getRequestId

-- =================================================================================

pipe, space, dash, colon, comma, quote, single, x :: Parser Char
pipe        = satisfy (== '|')
space       = satisfy (== ' ')
dash        = satisfy (== '-')
colon       = satisfy (== ':')
comma       = satisfy (== ',')
quote       = satisfy (== '"')
single      = satisfy (== '\'')
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
    separator
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
    separator
    return $ RequestId which (maybe 0 fst $ readInteger counter) (maybe 0 fst $ readInteger concurrent)

separator :: Parser Char
separator = do
    space
    pipe
    space


-- http: "GET /scm/CONF/confluence.git/info/refs HTTP/1.1"
-- ssh: git-upload-pack '/CONF/teamcal.git'
parseAction :: Parser Action
parseAction = choice [parseSshAction, parseHttpAction]

parseSshAction :: Parser Action
parseSshAction = do
    method <- takeTill (== ' ')
    space
    single
    path <- takeTill (== '\'')
    single
    separator
    return $ Action method path



parseHttpAction :: Parser Action
parseHttpAction = do
    quote
    method <- takeTill (== ' ')
    space
    path <- takeTill (== ' ')
    space
    _ <- takeTill (== '"')
    quote
    separator
    return $ Action method path


-- | Parse an access log line
parseLine :: Parser LogLine
parseLine = do
    remoteAddress <- logEntry
    protocol <- logEntry
    requestId <- parseRequestId
    rawUsername <- logEntry
    date <- parseLogEntryDate
    action <- parseAction
    details <- logEntry
    labels_ <- logEntry
    duration <- logEntry
    sessionId <- logEntry
    let labels = map trim $ split "," (S.unpack labels_)
        username = if rawUsername == "-" then Nothing else Just rawUsername
    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

-- | Remove leading and trailing whitespace
trim :: String -> String
trim str = T.unpack $ T.strip $ T.pack str

{-
Example log line
- 63.246.22.198,172.16.3.45 | https | o16x1402216x7 | cbac-confluence-user | 2012-09-08 00:17:00,270 | "GET /scm/ATLASSIAN/confluence.git/info/refs HTTP/1.1" | "" "JGit/unknown" | - | 1506 | - |

Example log line after upgrading the clone cache plugin
63.246.22.198,172.16.1.187 | https | o1116x10425x4 | cbac-confluence-user | 2012-10-10 18:36:02,078 | "GET /scm/ATLASSIAN/confluence.git/info/refs HTTP/1.1" | "" "JGit/unknown" | refs, cache:hit | 315 | s5ecvn |
-}
