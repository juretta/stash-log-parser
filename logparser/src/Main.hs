{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Stash.Log.Analyser hiding (ProtocolStats)
import Stash.Log.GitOpsAnalyser
import Stash.Log.Output
import Stash.Log.Input
import Control.Monad (liftM)
import System.Console.CmdArgs
import Prelude hiding (takeWhile)

-- =================================================================================

appName :: String
appName = "logparser"

appVersion :: String
appVersion = "1.9"

appShortDesc :: String
appShortDesc = "Logparser for the Atlassian Stash access logs"

data LogParser = MaxConn        {files :: [FilePath]}
                | CountRequests {files :: [FilePath]}
                | GitOperations {files :: [FilePath], progressive :: Bool}
                | GitDurations  {files :: [FilePath], progressive :: Bool}
                | ProtocolStats {files :: [FilePath]}
                | Count         {files :: [FilePath]}
                | DebugParser   {files :: [FilePath], progressive :: Bool}
             deriving (Data,Typeable,Show,Eq)

progressiveFlags :: Bool
progressiveFlags = False &= help "Progressively parse the logfiles" &= typ "BOOL"


maxConn :: LogParser
maxConn         = MaxConn {files = def &= args}
                &= name "maxConn"       &= help "Show the maximum number of concurrent requests per hour"

countRequests :: LogParser
countRequests   = CountRequests {files = def &= args}
                &= name "countRequests" &= help "Count the number of requests"

gitOperations :: LogParser
gitOperations   = GitOperations {files = def &= args, progressive = progressiveFlags}
                &= name "gitOperations" &= help "Aggregate git operations per hour. Show counts for fetch, clone, push, pull and ref advertisement"

gitDurations :: LogParser
gitDurations    = GitDurations {files = def &= args, progressive = progressiveFlags}
                &= name "gitDurations"  &= help "Show the duration of git operations over time"

protocolStats :: LogParser
protocolStats   = ProtocolStats {files = def &= args}
                &= name "protocolStats" &= help "Aggregate the number of git operations per hour based on the access protocol (http(s) vs. SSH)"

count :: LogParser
count           = Count {files = def &= args}
                &= name "count"         &= help "Count the number of lines in the given logfile(s)"

debugParser :: LogParser
debugParser     = DebugParser {files = def &= args, progressive = progressiveFlags}
                &= name "debugParser"   &= help "Parse and print the first five lines of the log file"


mode :: Mode (CmdArgs LogParser)
mode = cmdArgsMode $ modes [maxConn, countRequests, gitOperations, gitDurations, protocolStats, count, debugParser]
        &= help appShortDesc
        &= program appName &= summary (appName ++ " " ++ appVersion)
        &= verbosity


run :: LogParser -> IO ()
run (MaxConn files)                     = stream plotDataConcurrentConnHour printPlotDataConcurrentConn newRunConfig "printPlotDataConcurrentConn" files
run (CountRequests files)               = stream countRequestLines parseAndPrint newRunConfig "countRequestLines" files
run (GitOperations files progressive)   = stream analyseGitOperations printPlotDataGitOps (RunConfig progressive) "printPlotDataGitOps" files
run (GitDurations files progressive)    = stream gitRequestDuration printGitRequestDurations (RunConfig progressive) "gitRequestDuration" files
run (ProtocolStats files)               = stream protocolStatsByHour printProtocolData newRunConfig "printProtocolData" files
run (Count files)                       = printCountLines countLines files
run (DebugParser files progressive)     = stream showLines parseAndPrint newRunConfig "showLines" files

stream :: (Input -> a) -> (a -> IO ()) -> RunConfig -> String -> [FilePath] -> IO ()
stream analyze output runConfig name files = output =<< (liftM analyze $ readLogFiles runConfig name files)

main :: IO ()
main = do
    config <- cmdArgsRun mode
    run config
