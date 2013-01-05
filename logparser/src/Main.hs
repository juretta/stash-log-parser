{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Stash.Log.Analyser hiding (ProtocolStats)
import Stash.Log.GitOpsAnalyser
import Stash.Log.Output
import System.Console.CmdArgs
import Prelude hiding (takeWhile)

-- =================================================================================

appName :: String
appName = "logparser"

appVersion :: String
appVersion = "1.9"

appShortDesc :: String
appShortDesc = "Logparser for the Atlassian Stash access logs"

data LogParser = MaxConn {files :: [FilePath]}
                | CountRequests {files :: [FilePath]}
                | GitOperations {files :: [FilePath]}
                | GitDurations {files :: [FilePath]}
                | ProtocolStats {files :: [FilePath]}
                | Count {files :: [FilePath]}
                | DebugParser {files :: [FilePath]}
             deriving (Data,Typeable,Show,Eq)

maxConn :: LogParser
maxConn         = MaxConn {files = def &= args}
                &= name "maxConn"       &= help "Show the maximum number of concurrent requests per hour"

countRequests :: LogParser
countRequests   = CountRequests {files = def &= args}
                &= name "countRequests" &= help "Count the number of requests"

gitOperations :: LogParser
gitOperations   = GitOperations {files = def &= args}
                &= name "gitOperations" &= help "Aggregate git operations per hour. Show counts for fetch, clone, push, pull and ref advertisement"

gitDurations :: LogParser
gitDurations    = GitDurations {files = def &= args}
                &= name "gitDurations"  &= help "Show the duration of git operations over time"

protocolStats :: LogParser
protocolStats   = ProtocolStats {files = def &= args}
                &= name "protocolStats" &= help "Aggregate the number of git operations per hour based on the access protocol (http(s) vs. SSH)"

count :: LogParser
count           = Count {files = def &= args}
                &= name "count"         &= help "Count the number of lines in the given logfile(s)"

debugParser :: LogParser
debugParser     = DebugParser {files = def &= args}
                &= name "debugParser"   &= help "Parse and print the first five lines of the log file"


mode :: Mode (CmdArgs LogParser)
mode = cmdArgsMode $ modes [maxConn, countRequests, gitOperations, gitDurations, protocolStats, count, debugParser]
        &= help appShortDesc
        &= program appName &= summary (appName ++ " " ++ appVersion)


run :: LogParser -> IO ()
run (MaxConn files)         = printPlotDataConcurrentConn plotDataConcurrentConnHour files
run (CountRequests files)   = parseAndPrint countRequestLines files
run (GitOperations files)   = printPlotDataGitOps analyseGitOperations files
run (GitDurations files)    = printGitRequestDurations gitRequestDuration files
run (ProtocolStats files)   = printProtocolData protocolStatsByHour files
run (Count files)           = printCountLines countLines files
run (DebugParser files)     = parseAndPrint showLines files

main :: IO ()
main = do
    config <- cmdArgsRun mode
    run config
