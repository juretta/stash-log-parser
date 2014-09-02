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
) where

import           Data.Attoparsec.Char8      hiding (char, space, take, takeWhile)
import qualified Data.Attoparsec.Lazy       as AL
import           Data.ByteString.Char8      (readInt, readInteger)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Default
import           Data.Maybe                 (mapMaybe)
import qualified Data.String.Utils          as UT
import qualified Data.Text                  as T
import           Text.Printf                (printf)

type Input = [L.ByteString]

data Action = HttpAction {
    getMethod :: S.ByteString
  , getPath   :: S.ByteString
} | SshAction {
    getMethod :: S.ByteString
  , getPath   :: S.ByteString
} deriving (Show, Eq)

data InOurOut = In | Out deriving (Show, Eq)

newtype NodeId = NodeId S.ByteString deriving (Eq, Show)


data RequestId = RequestId {
    getInOrOut            :: !Char
  , getRequestCounter     :: !Integer
  , getConcurrentRequests :: !Integer
  , getMinuteOfTheDay     :: !Int
  , getNodeId             :: Maybe NodeId
  , isClustered           :: Bool
} deriving (Show, Eq)

instance Default RequestId where
    def = RequestId 'i' 0 0 0 Nothing False

data LogLine = LogLine {
    getRemoteAdress    :: S.ByteString
  , getProtocol        :: S.ByteString
  , getRequestId       :: RequestId
  , getUsername        :: Maybe S.ByteString
  , getDate            :: LogDate
  , getAction          :: Action
  , getDetails         :: S.ByteString
  , getLabels          :: [String]
  , getRequestDuration :: Maybe Int
  , getSessionId       :: S.ByteString
} deriving (Show, Eq)

data LogDate = LogDate {
    getYear    :: !Int
  , getMonth   :: !Int
  , getDay     :: !Int
  , getHour    :: !Int
  , getMinute  :: !Int
  , getSeconds :: !Int
  , getMillis  :: !Int
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
parseLogLine = AL.maybeResult . AL.parse parseLine

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

-- | Parse the request Id. A request id consist of a
--
--  * a char indicating an request 'i' or response 'o', followed by
--
--  For Stash 3.2 and later:
--
--  * a cluster node id prefixed with a char indicating whether the instance is
--    currently clustered or not ('*' if currently clustered or '@' if not.)
--
--
--  * the minute of the day,
--  * a request counter and
--  * the number of concurrent requests
--
--  separated by an 'x'.
--
--  E.g. i8x1401519x6 (for pre 3.2 versions of Stash) or o@2GNK8Mx1198x200381x2 for Stash 3.2 and later.
--
-- >>> AL.eitherResult $ AL.parse parseRequestId "i8x1401519x6"
-- Right (RequestId {getInOrOut = 'i', getRequestCounter = 1401519, getConcurrentRequests = 6, getMinuteOfTheDay = 8, getNodeId = Nothing, isClustered = False})
--
-- >>> AL.eitherResult $ AL.parse parseRequestId "o@2GNK8Mx1198x200381x5"
-- Right (RequestId {getInOrOut = 'o', getRequestCounter = 200381, getConcurrentRequests = 5, getMinuteOfTheDay = 1198, getNodeId = Just (NodeId "2GNK8M"), isClustered = False})
--
-- >>> AL.eitherResult $ AL.parse parseRequestId "i*2GNK8Mx1198x200381x22"
-- Right (RequestId {getInOrOut = 'i', getRequestCounter = 200381, getConcurrentRequests = 22, getMinuteOfTheDay = 1198, getNodeId = Just (NodeId "2GNK8M"), isClustered = True})
parseRequestId :: Parser RequestId
parseRequestId = do
    which <- satisfy (\c -> c == 'i' || c == 'o')

    next <- peekChar
    case next of
        Just c | c == '*' || c == '@' -> parseClusterNodeRequestId which
        _                             -> parsePre32RequestId which

  where
    parseClusterNodeRequestId which = do
        clusterStatus <- satisfy (\c -> c == '*' || c == '@')
        nodeId <- takeTill (== 'x')
        x
        minute <- decimal
        x
        counter <- takeTill (== 'x')
        x
        concurrent <- takeTill (== ' ')
        return $ def {
                     getInOrOut = which
                   , getRequestCounter = (toInteger' counter)
                   , getConcurrentRequests = (toInteger' concurrent)
                   , getMinuteOfTheDay = minute
                   , getNodeId = Just (NodeId nodeId)
                   , isClustered = if clusterStatus == '*' then True else False
               }
    parsePre32RequestId which = do
            minute <- decimal
            _ <- takeTill (== 'x')
            x
            counter <- takeTill (== 'x')
            x
            concurrent <- takeTill (== ' ')
            return $ def {
                         getInOrOut = which
                       , getRequestCounter = (toInteger' counter)
                       , getConcurrentRequests = (toInteger' concurrent)
                       , getMinuteOfTheDay = minute
                   }
    toInteger' = maybe 0 fst . readInteger

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
    return $ SshAction method path



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
    return $ HttpAction method path


-- | Parse an access log line
parseLine :: Parser LogLine
parseLine = do
    remoteAddress <- logEntry
    protocol <- logEntry
    requestId <- parseRequestId
    separator
    rawUsername <- logEntry
    date <- parseLogEntryDate
    action <- parseAction
    details <- logEntry
    labels_ <- logEntry
    duration <- parseDuration
    sessionId <- logEntry
    let labels = map trim $ UT.split "," (S.unpack labels_)
        username = if rawUsername == "-" then Nothing else Just rawUsername
    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

-- | Remove leading and trailing whitespace
trim :: String -> String
trim = T.unpack . T.strip . T.pack
