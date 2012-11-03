{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.GitOpsAnalyser
( GitOperationStats(..)
, plotGitOperations
) where

import qualified Data.ByteString.Char8 as S
import Data.List (foldl', groupBy)
import Data.Maybe (isJust)
import Data.Function (on)
import Text.Printf (printf)
import Stash.Log.Parser
import Stash.Log.Common (logDateEqHour)

data GitOperationStats = GitOperationStats {
     getOpStatDate              :: String
    ,cacheMisses                :: [Int] -- clone, fetch, shallow clone, push, ref advertisement
    ,cacheHits                  :: [Int]
}

emptyStats :: GitOperationStats
emptyStats = GitOperationStats "" zero zero
        where zero = replicate 5 0

-- As of 1.1.2 of the clone cache plugin, refs are explicitly listed in the
-- labels field, most of the data we have does _not_ have that information though
isRefAdvertisement :: LogLine -> Bool
isRefAdvertisement logLine = ".git/info/refs" `S.isSuffixOf` path && "GET" == method && isJust username
            where
                action      = getAction logLine
                path        = getPath action
                method      = getMethod action
                username    = getUsername logLine

isCacheHit :: LogLine -> Bool
isCacheHit logLine = inLabel logLine "cache:hit"

isCacheMiss :: LogLine -> Bool
isCacheMiss = not . flip inLabel "cache:hit" -- treat as cache miss if the cache:* label is missing

isFetch :: LogLine -> Bool
isFetch logLine = inLabel logLine "fetch" && not (inLabel logLine "clone" || inLabel logLine "shallow clone")

isClone :: LogLine -> Bool
isClone logLine = inLabel logLine "clone"

isShallowClone :: LogLine -> Bool
isShallowClone logLine = inLabel logLine "shallow clone"

isPush :: LogLine -> Bool
isPush logLine = inLabel logLine "push"

inLabel :: LogLine -> String -> Bool
inLabel logLine name =  let labels = getLabels logLine
                        in name `elem` labels
-- Example Output
-- (Date, clone, fetch, shallow clone, push)
plotGitOperations :: Input -> [GitOperationStats]
plotGitOperations rawLines =
    let formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date)
    in plotGitOperations' logDateEqHour formatLogDate rawLines

plotGitOperations' :: (LogDate -> LogDate -> Bool) -> (LogDate -> String) -> Input -> [GitOperationStats]
plotGitOperations' comp formatLogDate rawLines =
    let groups = groupBy (comp `on` getDate) $ parseLines rawLines
    in map (summarizeGitOperations formatLogDate) groups


cachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
cachedOperation op logLine = op logLine && isCacheHit logLine

uncachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
uncachedOperation op logLine = op logLine && isCacheMiss logLine

updateStats :: (LogDate -> String) -> GitOperationStats -> LogLine -> GitOperationStats
updateStats formatLogDate (GitOperationStats date misses hits) logLine =
                            let ops         = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
                                inc op      = if op logLine then (+1) else (+0)
                                missOps     = map (inc . uncachedOperation) ops
                                hitOps      = map (inc . cachedOperation) ops
                                !date'      = if null date then formatLogDate $ getDate logLine else date
                                !misses'    = zipWith id missOps misses
                                !hits'      = zipWith id hitOps hits
                            in GitOperationStats date' misses' hits'

summarizeGitOperations :: (LogDate -> String) -> [LogLine] -> GitOperationStats
summarizeGitOperations formatLogDate = foldl' aggregate emptyStats . filter isOutgoingLogLine
                        where aggregate = updateStats formatLogDate
