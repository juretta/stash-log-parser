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
    ,getDate                :: S.ByteString
    ,getAction              :: S.ByteString
    ,getDetails             :: S.ByteString
    ,getLabels              :: S.ByteString
    ,getRequestDuration     :: S.ByteString
    ,getSessionId           :: S.ByteString
} deriving (Show, Eq)

pipe, space, dash, quote, x :: Parser Char
pipe        = satisfy (== '|')
space       = satisfy (== ' ')
dash        = satisfy (== '-')
quote       = satisfy (== '\"')
x           = satisfy (== 'x')


logEntry :: Parser S.ByteString
logEntry = do
   entry <- takeTill (== '|')
   pipe
   space
   return entry

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
    date <- logEntry
    action <- logEntry
    details <- logEntry
    labels <- logEntry
    duration <- logEntry
    sessionId <- logEntry

    return $ LogLine remoteAddress protocol requestId username date
                    action details labels duration sessionId

countLines :: [L.ByteString] -> Integer
countLines = foldl' count' 0
    where
        count' acc l = case AL.maybeResult $ AL.parse line l of
            Just x  -> acc + 1
            Nothing -> acc


protocolCount :: [L.ByteString] -> [(S.ByteString,Integer)]
protocolCount = M.toList . foldl' count M.empty
        where
            count acc l = case AL.maybeResult $ AL.parse line l of
                Just x -> M.insertWith (+) (S.copy (getProtocol x)) 1 acc
                Nothing -> acc

showLines :: [L.ByteString] -> [Maybe LogLine]
showLines lines = take 5 $ map p_ lines
            where p_ = \x -> AL.maybeResult $ AL.parse line x

pretty :: Show a => Integer -> (a, Integer) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

-- =================================================================================

main :: IO ()
main = do
  [cmd,path] <- getArgs
  dispatch cmd path

dispatch :: Command -> FilePath -> IO ()
dispatch cmd = action
    where
        action = fromMaybe err (lookup cmd actions)
        err _  = putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

actions :: [(Command, FilePath -> IO ())]
actions = [("count", countLogFileLines)
          ,("show", showParsedLines)
          ,("protocol", mapToTopList protocolCount)]

showParsedLines :: FilePath -> IO()
showParsedLines path = parseAndPrint path showLines

countLogFileLines :: FilePath -> IO ()
countLogFileLines path = parseAndPrint path countLines

parseAndPrint :: (Show a) => FilePath -> ([L.ByteString] -> a) -> IO ()
parseAndPrint path f = print . f . L.lines =<< L.readFile path


mapToTopList :: ([L.ByteString] -> [(S.ByteString, Integer)]) -> FilePath -> IO ()
mapToTopList f p = do
    file <- liftM L.lines $ L.readFile p
    let mostPopular (_,a) (_,b) = compare b a
        m = f file
    mapM_ putStrLn . zipWith pretty [1..] . take 10 . sortBy mostPopular $ m
