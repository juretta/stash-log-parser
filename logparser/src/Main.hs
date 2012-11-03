module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import Stash.Log.Parser
import Stash.Log.Analyser
import Stash.Log.GitOpsAnalyser
import Data.Default
import UI.Command
import Prelude hiding (takeWhile)
import Text.Printf (printf)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

-- =================================================================================

logparser :: Application () ()
logparser = def {
                appName = "logparser",
                appVersion = "1.4",
                appAuthors = ["Stefan Saasen"],
                appBugEmail = "ssaasen@atlassian.com",
                appCategories = ["Logfile analysis", "Debug"],
                appShortDesc = "Logparser for the Atlassian Stash access logs",
                appLongDesc = "Parses and aggregates the access logs of Atlassian Stash",
                appCmds = [count, countRequests, maxConn, summarizeGitOperations, summarizeProtocolStats, debugParser ]
        }


count, countRequests, maxConn, summarizeGitOperations, summarizeProtocolStats, debugParser :: Command ()
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
        (file:_)   -> liftIO $ f file
        []          -> error "Path to logfile is missing"


main :: IO ()
main = appMain logparser

-- =================================================================================
--
-- =================================================================================

generateProtocolData :: (Input -> [ProtocolStats]) -> FilePath -> IO ()
generateProtocolData f path = do
        plotData <- liftM f $ toLines path
        mapM_ (\(ProtocolStats date ssh http) -> printf "%s|%d|%d\n" date ssh http) plotData

generatePlotDataGitOps :: (Input -> [GitOperationStats]) -> FilePath -> IO ()
generatePlotDataGitOps f path = do
        plotData <- liftM f $ toLines path
        mapM_ (\(GitOperationStats date [a,b,c,d,e] [aHit,bHit,cHit,dHit,eHit]) -- clone, fetch, shallow clone, push, ref advertisement
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d\n" date (a+aHit) (b+bHit) (c+cHit) (d+dHit) (e+eHit) aHit bHit cHit dHit eHit a b c d e) plotData

generatePlotDataConcurrentConn :: (Input -> [DateValuePair]) -> FilePath -> IO ()
generatePlotDataConcurrentConn f path = do
        plotData <- liftM f $ toLines path
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

parseAndPrint :: (Show a) => (Input -> a) -> FilePath -> IO ()
parseAndPrint f path = print . f . L.lines =<< L.readFile path

printCountLines :: (Show a) => (L.ByteString -> a) -> FilePath -> IO ()
printCountLines f path = print . f =<< L.readFile path

toLines :: FilePath -> IO [L.ByteString]
toLines path = liftM L.lines $ L.readFile path

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)

