{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Parser
( Action(..)
, InOurOut
, RequestId(..)
, LogLine(..)
, LogDate(..)
, Input
, parseLogLine
, parseLogLines
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
import Data.ByteString.Char8 (readInteger, readInt)
import Data.String.Utils (split)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

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
    ,getRequestDuration     :: Maybe Int
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
} deriving (Eq)

instance Show LogDate where
    show date = printf "%04d-%02d-%02d %02d:%02d:%02d" (getYear date)
                                (getMonth date)
                                (getDay date)
                                (getHour date)
                                (getMinute date)
                                (getSeconds date)

-- | Parse the input into a list of LogLines
parseLogLines :: Input -> [LogLine]
parseLogLines = mapMaybe parseLogLine

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
   entry <- parseEntry
   return $ S.init entry

parseDuration :: Parser (Maybe Int)
parseDuration = do
    entry <- parseEntry
    return $ fmap fst (readInt entry)

parseEntry :: Parser S.ByteString
parseEntry = do
    entry <- takeTill (== '|')
    pipe
    space
    return entry

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
-- ssh (new format): SSH - git-upload-pack '/CONF/teamcal.git'
parseAction :: Parser Action
parseAction = choice [parseSshAction, parseHttpAction]

parseSshAction :: Parser Action
parseSshAction = do
    method <- takeTill (== '\'')
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
    duration <- parseDuration
    sessionId <- logEntry
    let labels = map trim $ split "," (S.unpack labels_)
        username = if rawUsername == "-" then Nothing else Just rawUsername
    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

-- | Remove leading and trailing whitespace
trim :: String -> String
trim = T.unpack . T.strip . T.pack
