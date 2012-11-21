{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Codec.Compression.BZip as BZip
import Stash.Log.Parser
import Stash.Log.Analyser
import Stash.Log.GitOpsAnalyser
import Data.Default
import Data.List (isSuffixOf)
import UI.Command
import Prelude hiding (takeWhile)
import Text.Printf (printf)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

-- =================================================================================

logparser :: Application () ()
logparser = def {
                appName = "logparser",
                appVersion = "1.6",
                appAuthors = ["Stefan Saasen"],
                appBugEmail = "ssaasen@atlassian.com",
                appCategories = ["Logfile analysis", "Debug"],
                appShortDesc = "Logparser for the Atlassian Stash access logs",
                appLongDesc = "Parses and aggregates the access logs of Atlassian Stash",
                appCmds = [count, countRequests, maxConn, summarizeGitOperations, requestDurations, summarizeProtocolStats, debugParser ]
        }


count, countRequests, maxConn, summarizeGitOperations, requestDurations, summarizeProtocolStats, debugParser :: Command ()
count = defCmd {
                cmdName = "count",
                cmdHandler = commandHandler $ printCountLines countLines,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Count the number of lines in the given logfile"
        }

countRequests = defCmd {
                cmdName = "countRequests",
                cmdHandler = commandHandler $ parseAndPrint countRequestLines,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Count the number requests"
        }

maxConn = defCmd {
                cmdName = "maxConn",
                cmdHandler = commandHandler $ generatePlotDataConcurrentConn plotDataConcurrentConnHour,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Show the maximum number of concurrent requests per hour"
        }

summarizeGitOperations = defCmd {
                cmdName = "gitOperations",
                cmdHandler = commandHandler $ generatePlotDataGitOps analyseGitOperations,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Aggregate git operations per hour. Show counts for fetch, clone, push, pull and ref advertisement"
        }

requestDurations = defCmd {
                cmdName = "requestDurations",
                cmdHandler = commandHandler $ generateCloneRequestDurations cloneRequestDuration,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Show the duration of clone operations over time"
        }

summarizeProtocolStats = defCmd {
                cmdName = "protocolStats",
                cmdHandler = commandHandler $ generateProtocolData protocolStatsByHour,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Aggregate the number of git operations per hour based on the access protocol (http(s) vs. SSH)"
        }

debugParser = defCmd {
                cmdName = "debugParser",
                cmdHandler = commandHandler $ parseAndPrint showLines,
                cmdCategory = "Debug",
                cmdShortDesc = "Parse and print the first five lines of the log file"
        }

commandHandler f = do
    args <- appArgs
    case args of
        []          -> error "Path to logfile(s) is missing"
        files       -> liftIO $ f files


main :: IO ()
main = appMain logparser

-- =================================================================================
--
-- =================================================================================

generateProtocolData :: (Input -> [ProtocolStats]) -> [FilePath] -> IO ()
generateProtocolData f path = do
        plotData <- liftM f $ toLines path
        printf "# Date | SSH | HTTP(s)\n"
        mapM_ (\(ProtocolStats date ssh http) -> printf "%s|%d|%d\n" date ssh http) plotData

generatePlotDataGitOps :: (Input -> [GitOperationStats]) -> [FilePath] -> IO ()
generatePlotDataGitOps f path = do
        plotData <- liftM f $ toLines path
        printf "# Date | clone | fetch | shallow clone | push | ref advertisement | clone (hit) | fetch (hit) | shallow clone (hit) | push (hit) | ref advertisement (hit) | clone (miss) | fetch (miss) | shallow clone (miss) | push (miss) | ref advertisement (miss)\n"
        mapM_ (\(GitOperationStats date [a,b,c,d,e] [aHit,bHit,cHit,dHit,eHit])
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d\n" date (a+aHit) (b+bHit) (c+cHit) (d+dHit) (e+eHit) aHit bHit cHit dHit eHit a b c d e) plotData

generatePlotDataConcurrentConn :: (Input -> [DateValuePair]) -> [FilePath] -> IO ()
generatePlotDataConcurrentConn f path = do
        plotData <- liftM f $ toLines path
        printf "# Date | Max concurrent connection\n"
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

generateCloneRequestDurations :: (Input -> [RequestDurationStat]) -> [FilePath] -> IO ()
generateCloneRequestDurations g path = do
        plotData <- liftM g $ toLines path
        printf "# Date | Clone duration (cache hit) | Clone duration (cache miss) | Fetch (hit) | Fetch (miss) | Shallow Clone (hit) | Shallow Clone (miss) | Push (hit) | Push (miss) | Ref adv (hit) | Ref adv (miss) | Client IP\n"
        mapM_ (\(RequestDurationStat date clientIp [cm,fm,sm,pm,rm] [c,f,s,p,r])
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%s\n" (show date) c cm f fm s sm p pm r rm clientIp) plotData

parseAndPrint :: (Show a) => (Input -> a) -> [FilePath] -> IO ()
parseAndPrint f path = print . f . L.lines =<< readFiles path

printCountLines :: (Show a) => (L.ByteString -> a) -> [FilePath] -> IO ()
printCountLines f path = print . f =<< readFiles path

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)

-- =================================================================================

toLines :: [FilePath] -> IO [L.ByteString]
toLines = liftM L.lines . readFiles

readFiles :: [FilePath] -> IO L.ByteString
readFiles = fmap L.concat . mapM readCompressedOrUncompressed

readCompressedOrUncompressed :: FilePath -> IO L.ByteString
readCompressedOrUncompressed path = if ".bz2" `isSuffixOf` path
                                    then liftM BZip.decompress $ L.readFile path
                                    else L.readFile path
