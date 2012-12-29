{-# LANGUAGE OverloadedStrings #-}
module Main where

import Stash.Log.Analyser
import Stash.Log.GitOpsAnalyser
import Stash.Log.Output
import Data.Default
import UI.Command
import Prelude hiding (takeWhile)
import Control.Monad.Trans (liftIO)

-- =================================================================================

logparser :: Application () ()
logparser = def {
                appName = "logparser",
                appVersion = "1.9",
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
                cmdHandler = commandHandler $ printPlotDataConcurrentConn plotDataConcurrentConnHour,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Show the maximum number of concurrent requests per hour"
        }

summarizeGitOperations = defCmd {
                cmdName = "gitOperations",
                cmdHandler = commandHandler $ printPlotDataGitOps analyseGitOperations,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Aggregate git operations per hour. Show counts for fetch, clone, push, pull and ref advertisement"
        }

requestDurations = defCmd {
                cmdName = "requestDurations",
                cmdHandler = commandHandler $ printCloneRequestDurations cloneRequestDuration,
                cmdCategory = "Logfile analysis",
                cmdShortDesc = "Show the duration of clone operations over time"
        }

summarizeProtocolStats = defCmd {
                cmdName = "protocolStats",
                cmdHandler = commandHandler $ printProtocolData protocolStatsByHour,
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



-- =================================================================================



