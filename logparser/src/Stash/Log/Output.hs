{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stash.Log.Output
( printProtocolData
, printGitRequestDurations
, printPlotDataConcurrentConn
, printPlotDataGitOps
, printCountLines
, printRepoStatsData
, printRequestClassification
) where


import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Stash.Log.Analyser
import           Stash.Log.GitOpsAnalyser
import           Stash.Log.Input
import           Stash.Log.Parser
import           Text.Printf


formatLogDateHour :: LogDate -> String
formatLogDateHour date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date) (getDay date) (getHour date) (getMinute date)

printRequestClassification :: RequestClassification -> IO ()
printRequestClassification RequestClassification{..} = do
    let sum' = fromIntegral (gitHttp + gitSsh + webUi + fileServer + rest) :: Double
    printf "RequestType | Count | Percent\n"
    printf "Git HTTP | %d | %.2f\n"        gitHttp (fromIntegral gitHttp/sum')
    printf "Git SSH | %d | %.2f\n"         gitSsh (fromIntegral gitSsh/sum')
    printf "Web UI | %d | %.2f\n"          webUi (fromIntegral webUi/sum')
    printf "File server | %d | %.2f\n"     fileServer (fromIntegral fileServer/sum')
    printf "REST | %d | %.2f\n"            rest (fromIntegral rest/sum')

printProtocolData :: [ProtocolStats] -> IO ()
printProtocolData plotData = do
        printf "# Date | SSH | HTTP(s)\n"
        mapM_ (\(ProtocolStats date ssh http) -> printf "%s|%d|%d\n" (formatLogDateHour date) ssh http) plotData

printPlotDataGitOps :: [GitOperationStats] -> IO ()
printPlotDataGitOps plotData = do
        printf "# Date | clone | fetch | shallow clone | push | ref advertisement | clone (hit) | fetch (hit) | shallow clone (hit) | push (hit) | ref advertisement (hit) | clone (miss) | fetch (miss) | shallow clone (miss) | push (miss) | ref advertisement (miss)\n"
        mapM_ (\(GitOperationStats date [a,b,c,d,e] [aHit,bHit,cHit,dHit,eHit])
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d\n" (formatLogDateHour date) (a+aHit) (b+bHit) (c+cHit) (d+dHit) (e+eHit) aHit bHit cHit dHit eHit a b c d e) plotData

printPlotDataConcurrentConn :: [DateValuePair] -> IO ()
printPlotDataConcurrentConn plotData = do
        printf "# Date | Max concurrent connection\n"
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

printGitRequestDurations :: [RequestDurationStat] -> IO ()
printGitRequestDurations plotData = do
        printf "# Date | Clone duration (cache hit) | Clone duration (cache miss) | Fetch (hit) | Fetch (miss) | Shallow Clone (hit) | Shallow Clone (miss) | Push (hit) | Push (miss) | Ref adv (hit) | Ref adv (miss) | Client IP | Username \n"
        mapM_ (\(RequestDurationStat date clientIp [cm,fm,sm,pm,rm] [c,f,s,p,r] username)
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%s|%s\n" (formatLogDateHour date) c cm f fm s sm p pm r rm (S.unpack clientIp) (S.unpack username)) plotData

printRepoStatsData :: [RepositoryStat] -> IO ()
printRepoStatsData xs = do
        printf "# Repository name | Number of clones \n"
        mapM_ p xs
    where p (RepositoryStat name num) = printf "%s|%d\n" (S.unpack name) num
          p _                         = printf "N/A|0\n"

printCountLines :: (Show a) => (L.ByteString -> a) -> [FilePath] -> IO ()
printCountLines f path = print . f =<< readFiles path

-- =================================================================================
formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)


-- instance PrintfArg Millis where
