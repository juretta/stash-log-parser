{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Output
( printProtocolData
, printGitRequestDurations
, printPlotDataConcurrentConn
, printPlotDataGitOps
, parseAndPrint
, printCountLines
) where


import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import Control.Monad (liftM)
import Stash.Log.Parser
import Stash.Log.Analyser
import Stash.Log.GitOpsAnalyser
import Stash.Log.File
import Text.Printf (printf)
import Data.Aeson (decode)


printProtocolData :: (Input -> [ProtocolStats]) -> [FilePath] -> IO ()
printProtocolData f path = do
        plotData <- liftM f $ readLogFiles "printProtocolData" path
        printf "# Date | SSH | HTTP(s)\n"
        mapM_ (\(ProtocolStats date ssh http) -> printf "%s|%d|%d\n" date ssh http) plotData

printPlotDataGitOps :: (Input -> [GitOperationStats]) -> [FilePath] -> IO ()
printPlotDataGitOps f path = do
        plotData <- liftM f $ readLogFiles "printPlotDataGitOps" path
        printf "# Date | clone | fetch | shallow clone | push | ref advertisement | clone (hit) | fetch (hit) | shallow clone (hit) | push (hit) | ref advertisement (hit) | clone (miss) | fetch (miss) | shallow clone (miss) | push (miss) | ref advertisement (miss)\n"
        mapM_ (\(GitOperationStats date [a,b,c,d,e] [aHit,bHit,cHit,dHit,eHit])
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d\n" date (a+aHit) (b+bHit) (c+cHit) (d+dHit) (e+eHit) aHit bHit cHit dHit eHit a b c d e) plotData

printPlotDataConcurrentConn :: (Input -> [DateValuePair]) -> [FilePath] -> IO ()
printPlotDataConcurrentConn f path = do
        plotData <- liftM f $ readLogFiles "printPlotDataConcurrentConn" path
        printf "# Date | Max concurrent connection\n"
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

printGitRequestDurations :: (Input -> [RequestDurationStat]) -> [FilePath] -> IO ()
printGitRequestDurations g path = do
        plotData <- liftM g $ readLogFiles "printCloneRequestDurations" path
        printf "# Date | Clone duration (cache hit) | Clone duration (cache miss) | Fetch (hit) | Fetch (miss) | Shallow Clone (hit) | Shallow Clone (miss) | Push (hit) | Push (miss) | Ref adv (hit) | Ref adv (miss) | Client IP | Username \n"
        mapM_ (\(RequestDurationStat date clientIp [cm,fm,sm,pm,rm] [c,f,s,p,r] username)
                -> printf "%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%s|%s\n" (show date) c cm f fm s sm p pm r rm clientIp (S.unpack username)) plotData

parseAndPrint :: (Show a) => (Input -> a) -> [FilePath] -> IO ()
parseAndPrint f path = print . f . L.lines =<< readFiles (const True) path

printCountLines :: (Show a) => (L.ByteString -> a) -> [FilePath] -> IO ()
printCountLines f path = print . f =<< readFiles (const True) path

-- =================================================================================
formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)

readConfig :: String -> IO (Maybe String)
readConfig key = do
        json <- L.readFile "logparser.state"
        return $ (decode json :: Maybe (M.Map String String)) >>= M.lookup key

readLogFiles :: String -> [FilePath] -> IO [L.ByteString]
readLogFiles key path = do
        date <- readConfig key
        toLines (createPredicate date) path
        where createPredicate = maybe (const True) $ flip isFileNewer
