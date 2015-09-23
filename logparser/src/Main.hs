{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad.Reader
import           Prelude                  hiding (takeWhile)
import           Stash.Log.Analyser
import           Stash.Log.Chart
import qualified Stash.Log.GitOpsAnalyser as G
import           Stash.Log.Input          (readLogFiles)
import           Stash.Log.Output
import           Stash.Log.Types
import           System.Console.CmdArgs
import           System.Environment       (getArgs, withArgs)

-- =================================================================================

appName :: String
appName = "logparser"

appVersion :: String
appVersion = "3.0"

appShortDesc :: String
appShortDesc = "Logparser for the Atlassian Stash & Bitbucket Server access logs"

data LogParserRunMode =
                  MaxConn           {files :: [FilePath], graph :: Bool, targetDir :: FilePath}
                | CountRequests     {files :: [FilePath]}
                | GitOperations     {files :: [FilePath], graph :: Bool, targetDir :: FilePath, aggregationLevel :: AggregationLevel}
                | GitDurations      {files :: [FilePath], targetDir :: FilePath}
            --  | GitDurations      {files :: [FilePath], graph :: Bool, targetDir :: FilePath}
                | ProtocolStats     {files :: [FilePath], graph :: Bool, targetDir :: FilePath}
                | RepositoryStats   {files :: [FilePath], graph :: Bool, targetDir :: FilePath}
                | Count             {files :: [FilePath]}
                | Classification    {files :: [FilePath], graph :: Bool, targetDir :: FilePath}
                | DebugParser       {files :: [FilePath]}
                | All               {files :: [FilePath], targetDir :: FilePath, aggregationLevel :: AggregationLevel}
             deriving (Data,Typeable,Show,Eq)


graphFlag :: Bool
graphFlag = False &= help "Render graphs instead of printing statistics to STDOUT" &= typ "BOOL" &= groupname "Output"

outputDirFlag :: FilePath
outputDirFlag = def &= typDir &= help "Directory to be used to store the generated graphs. Default: working directory" &= groupname "Output"

aggregationLevelFlag :: AggregationLevel
aggregationLevelFlag = def &= help "Values will be aggregated by hour (default) or by minute" &= opt Hour &= groupname "Statistics" &= explicit &= name "aggregate" &= typ "Hour|Minute"

maxConn :: LogParserRunMode
maxConn         = MaxConn {files = def &= args, graph = graphFlag, targetDir = outputDirFlag}
                &= name "maxConn"       &= help "Show the maximum number of concurrent requests per hour"

countRequests :: LogParserRunMode
countRequests   = CountRequests {files = def &= args}
                &= name "countRequests" &= help "Count the number of requests"

gitOperations :: LogParserRunMode
gitOperations   = GitOperations {files = def &= args, graph = graphFlag, targetDir = outputDirFlag, aggregationLevel = aggregationLevelFlag}
                &= name "gitOperations" &= help "Aggregate git operations per hour or minute. Show counts for fetch, clone, push, pull and ref advertisement"

gitDurations :: LogParserRunMode
gitDurations    = GitDurations {files = def &= args, targetDir = outputDirFlag}
                &= name "gitDurations"  &= help "Show the duration of git operations over time"

protocolStats :: LogParserRunMode
protocolStats   = ProtocolStats {files = def &= args, graph = graphFlag, targetDir = outputDirFlag}
                &= name "protocolStats" &= help "Aggregate the number of git operations per hour based on the access protocol (http(s) vs. SSH)"

repositoryStats :: LogParserRunMode
repositoryStats = RepositoryStats {files = def &= args, graph = graphFlag, targetDir = outputDirFlag}
                &= name "repositoryStats" &= help "Show the number of git clone \
                    \operations per repository"

requestClassification :: LogParserRunMode
requestClassification = Classification {files = def &= args, graph = graphFlag, targetDir = outputDirFlag}
                        &= name "requestClassification" &= help "Classify the different request types (REST, FileServer, Web UI, Git HTTP, Git SSH)"

count :: LogParserRunMode
count           = Count {files = def &= args}
                &= name "count"         &= help "Count the number of lines in the given logfile(s)"

debugParser :: LogParserRunMode
debugParser     = DebugParser {files = def &= args}
                &= name "debugParser"   &= help "Parse and print the first five lines of the log file"

allOps :: LogParserRunMode
allOps = All {files = def &= args, targetDir = outputDirFlag, aggregationLevel = aggregationLevelFlag}
      &= name "all" &= help "Generate all available graphs for the given input files"

mode :: Mode (CmdArgs LogParserRunMode)
mode = cmdArgsMode $ modes [maxConn, countRequests, gitOperations &= auto, gitDurations, -- // chart generation is currently disabled as it is too slow
                            protocolStats, repositoryStats, requestClassification, count, allOps, debugParser]
        &= help appShortDesc
        &= helpArg [explicit, name "help", name "h"]
        &= program appName &= summary (appName ++ " " ++ appVersion)
        &= verbosity


run :: LogParserRunMode -> IO ()
run (MaxConn files' False _)                     = stream concurrentConnections printPlotDataConcurrentConn files'
run (MaxConn files' True targetDir')             = genMaxConnectionChart targetDir' files'
run (CountRequests files')                       = stream countRequestLines print files'
run (GitOperations files' False _ lvl)           = stream (G.analyseGitOperations lvl) printPlotDataGitOps files'
run (GitOperations files' True targetDir' lvl)   = genGitOperationsChart lvl targetDir' files'
run (GitDurations files'       _)                = stream G.gitRequestDuration printGitRequestDurations files'
{-run (GitDurations files' targetDir')             = genGitDurationChart targetDir' files'-}
run (Classification files' True targetDir')      = genRequestClassificationChart targetDir' files'
run (Classification files' False _)              = stream classifyRequests printRequestClassification files'
run (ProtocolStats files' False _)               = stream G.protocolStatsByHour printProtocolData files'
run (ProtocolStats files' True targetDir')       = genProtocolStatsGraph targetDir' files'
run (RepositoryStats files' False _)             = stream G.repositoryStats printRepoStatsData files'
run (RepositoryStats files' True targetDir')     = genRepositoryStatsGraph targetDir' files'
run (Count files')                               = printCountLines countLines files'
run (DebugParser files')                         = stream showLines print files'
run (All files' targetDir' lvl)                  = do

  putStrLn "Starting to analyze the given access log(s)"
  

  let actions = [genMaxConnectionChart
                , genGitOperationsChart lvl
                -- , genGitDurationChart
                , genRequestClassificationChart
                , genProtocolStatsGraph,
                genRepositoryStatsGraph]
  mapM_ (\action -> do
        action targetDir' files'
        putStrLn "Generating next graph"
        ) actions

genMaxConnectionChart :: [Char] -> [FilePath] -> IO ()
genMaxConnectionChart targetDir' files' = do
    let outputF = generateMaxConnectionChart "maxConnection" targetDir'
    stream concurrentConnections outputF files'

genGitOperationsChart :: AggregationLevel
                               -> [Char] -> [FilePath] -> IO ()
genGitOperationsChart lvl targetDir' files' = do
    let outputF = generateGitOperationsChart "gitOperations" targetDir'
    stream (G.analyseGitOperations lvl) outputF files'

genGitDurationChart :: [Char] -> [FilePath] -> IO ()
genGitDurationChart targetDir' files' = do
    let outputF = generateGitDurationChart "gitDurations" targetDir'
    stream G.gitRequestDuration outputF files'

genRequestClassificationChart :: [Char] -> [FilePath] -> IO ()
genRequestClassificationChart targetDir' files' = do
    let outputF = generateRequestClassificationChart "requestClassification" targetDir'
    stream classifyRequests outputF files'

genProtocolStatsGraph :: [Char] -> [FilePath] -> IO ()
genProtocolStatsGraph targetDir' files' = do
    let outputF = generateProtocolStats "protocolStats" targetDir'
    stream G.protocolStatsByHour outputF files'

genRepositoryStatsGraph :: [Char] -> [FilePath] -> IO ()
genRepositoryStatsGraph targetDir' files' = do
    let outputF = generateRepositoryStats "repositoryStats" targetDir'
    stream G.repositoryStats outputF files'


stream :: (Input -> a) -> (a -> IO ()) -> [FilePath] -> IO ()
stream analyze output files' = output =<< liftM analyze (readLogFiles files')

main :: IO ()
main = do
    options <- getArgs
    -- We need arguments so if there are no arguments given, invoke the help command
    config <- (if null options then withArgs ["--help"] else id) $ cmdArgsRun mode
    run config


instance Default AggregationLevel where
    def = Hour
