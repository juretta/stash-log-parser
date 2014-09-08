{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad.Reader
import           Prelude                  hiding (takeWhile)
import           Stash.Log.Analyser
import           Stash.Log.Chart
import qualified Stash.Log.GitOpsAnalyser as G
import           Stash.Log.Input
import           Stash.Log.Output
import           Stash.Log.Types
import           System.Console.CmdArgs
import           System.Environment       (getArgs, withArgs)

-- =================================================================================

appName :: String
appName = "logparser"

appVersion :: String
appVersion = "2.0"

appShortDesc :: String
appShortDesc = "Logparser for the Atlassian Stash access logs"

data LogParserRunMode =
                  MaxConn           {files :: [FilePath]}
                | CountRequests     {files :: [FilePath]}
                | GitOperations     {files :: [FilePath], progressive :: Bool, graph :: Bool, targetDir :: FilePath, aggregationLevel :: AggregationLevel}
                | GitDurations      {files :: [FilePath], progressive :: Bool, graph :: Bool, targetDir :: FilePath}
                | ProtocolStats     {files :: [FilePath]}
                | RepositoryStats   {files :: [FilePath]}
                | Count             {files :: [FilePath]}
                | DebugParser       {files :: [FilePath], progressive :: Bool}
             deriving (Data,Typeable,Show,Eq)

progressiveFlags :: Bool
progressiveFlags = False &= help "Progressively parse the logfiles" &= typ "BOOL"

graphFlag :: Bool
graphFlag = False &= help "Render graphs instead of printing statistics to STDOUT" &= typ "BOOL" &= groupname "Output"

outputDirFlag :: FilePath
outputDirFlag = def &= typDir &= help "Directory to be used to store the generated graphs. Default: working directory" &= groupname "Output"

aggregationLevelFlag :: AggregationLevel
aggregationLevelFlag = def &= help "Values will be aggregated by hour (default) or by minute" &= opt Hour &= groupname "Statistics" &= explicit &= name "level" &= typ "Hour|Minute"

maxConn :: LogParserRunMode
maxConn         = MaxConn {files = def &= args}
                &= name "maxConn"       &= help "Show the maximum number of concurrent requests per hour"

countRequests :: LogParserRunMode
countRequests   = CountRequests {files = def &= args}
                &= name "countRequests" &= help "Count the number of requests"

gitOperations :: LogParserRunMode
gitOperations   = GitOperations {files = def &= args, progressive = progressiveFlags, graph = graphFlag, targetDir = outputDirFlag, aggregationLevel = aggregationLevelFlag}
                &= name "gitOperations" &= help "Aggregate git operations per hour or minute. Show counts for fetch, clone, push, pull and ref advertisement"

gitDurations :: LogParserRunMode
gitDurations    = GitDurations {files = def &= args, progressive = progressiveFlags, graph = graphFlag, targetDir = outputDirFlag}
                &= name "gitDurations"  &= help "Show the duration of git operations over time"

protocolStats :: LogParserRunMode
protocolStats   = ProtocolStats {files = def &= args}
                &= name "protocolStats" &= help "Aggregate the number of git operations per hour based on the access protocol (http(s) vs. SSH)"

repositoryStats :: LogParserRunMode
repositoryStats = RepositoryStats {files = def &= args}
                &= name "repositoryStats" &= help "Show the number of git clone \
                    \operations per repository"

count :: LogParserRunMode
count           = Count {files = def &= args}
                &= name "count"         &= help "Count the number of lines in the given logfile(s)"

debugParser :: LogParserRunMode
debugParser     = DebugParser {files = def &= args, progressive = progressiveFlags}
                &= name "debugParser"   &= help "Parse and print the first five lines of the log file"


mode :: Mode (CmdArgs LogParserRunMode)
mode = cmdArgsMode $ modes [maxConn, countRequests, gitOperations, gitDurations,
                            protocolStats, repositoryStats, count, debugParser]
        &= help appShortDesc
        &= helpArg [explicit, name "help", name "h"]
        &= program appName &= summary (appName ++ " " ++ appVersion)
        &= verbosity


run :: LogParserRunMode -> IO ()
run (MaxConn files')                     = stream concurrentConnections printPlotDataConcurrentConn newRunConfig "printPlotDataConcurrentConn" files'
run (CountRequests files')               = stream countRequestLines print newRunConfig "countRequestLines" files'
run (GitOperations files' progressive' False _ lvl) = stream (G.analyseGitOperations lvl) printPlotDataGitOps (RunConfig progressive') "printPlotDataGitOps" files'
run (GitOperations files' progressive' True targetDir' lvl) = do
    let outputF = generateGitOperationsChart "gitOperations" targetDir'
    stream (G.analyseGitOperations lvl) outputF (RunConfig progressive') "printPlotDataGitOps" files'
run (GitDurations files' progressive' False _)   = stream G.gitRequestDuration printGitRequestDurations (RunConfig progressive') "gitRequestDuration" files'
run (GitDurations files' progressive' True targetDir') = do
    let outputF = generateGitDurationChart "gitDurations" targetDir'
    stream G.gitRequestDuration outputF (RunConfig progressive') "printPlotDataGitOps" files'
run (ProtocolStats files')               = stream G.protocolStatsByHour printProtocolData newRunConfig "printProtocolData" files'
run (RepositoryStats files')             = stream G.repositoryStats printRepoStatsData newRunConfig "printRepoStatsData" files'
run (Count files')                       = printCountLines countLines files'
run (DebugParser files' progressive')    = stream showLines print (RunConfig progressive') "showLines" files'

stream :: (Input -> a) -> (a -> IO ()) -> RunConfig -> String -> [FilePath] -> IO ()
stream analyze output runConfig name' files' = output =<< liftM analyze (readLogFiles runConfig name' files')

main :: IO ()
main = do
    options <- getArgs
    -- We need arguments so if there are no arguments given, invoke the help command
    config <- (if null options then withArgs ["--help"] else id) $ cmdArgsRun mode
    run config


instance Default AggregationLevel where
    def = Hour
