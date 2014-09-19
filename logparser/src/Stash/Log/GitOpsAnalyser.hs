{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stash.Log.GitOpsAnalyser
(
  GitOperationStats(..)
, RequestDurationStat(..)
, RepositoryStat(..)
, ProtocolStats(..)

, analyseGitOperations
, gitRequestDuration
, protocolStatsByHour
, repositoryStats


 -- Exported for testing
, isRefAdvertisement
, isClone
, isShallowClone
, isPush
, extractRepoSlug
) where

import qualified Data.ByteString.Char8 as S
import           Data.Char             (toLower)
import           Data.Default
import           Data.Function         (on)
import           Data.List             (foldl', groupBy, sortBy)
import           Data.Maybe            (mapMaybe, fromMaybe)
import           Stash.Log.Common
import           Stash.Log.Parser
import           Stash.Log.Types
import           Control.DeepSeq


data GitOperationStats = GitOperationStats {
    getOpStatDate :: !LogDate
  , cacheMisses   :: ![Int] -- clone, fetch, shallow clone, push, ref advertisement
  , cacheHits     :: ![Int]
} deriving (Show)

data RequestDurationStat = RequestDurationStat {
    getDurationDate    :: !LogDate
  , getClientIp        :: !S.ByteString
  , cacheMissDurations :: ![Millis]
  , cacheHitDurations  :: ![Millis] -- clone, fetch, shallow clone, push, ref advertisement
  , requestUsername    :: !S.ByteString
}

data ProtocolStats = ProtocolStats {
    getProtocolLogDate :: !LogDate
  , getSsh             :: !Int
  , getHttp            :: !Int
}

data RepositoryStat = RepositoryStat {
    getName           :: !S.ByteString
  , getNumberOfClones :: !Int
} | StatUnavailable deriving (Show)

instance NFData GitOperationStats where
    rnf gos@GitOperationStats{..} =
        gos {
            getOpStatDate = getOpStatDate `seq` getOpStatDate
          , cacheMisses = cacheMisses `deepseq` cacheMisses
          , cacheHits = cacheHits `deepseq` cacheHits
        } `seq` ()

instance NFData RequestDurationStat where
    rnf rd@RequestDurationStat{..} =
        rd {
            getDurationDate = getDurationDate `seq` getDurationDate
          , getClientIp = getClientIp `seq` getClientIp
          , cacheMissDurations = cacheMissDurations `deepseq` cacheMissDurations
          , cacheHitDurations = cacheHitDurations `deepseq` cacheHitDurations
          , requestUsername = requestUsername `seq` requestUsername
        } `seq` ()

-- | Parse and aggregate the log file input into a list of hourly GitOperationStats
analyseGitOperations :: AggregationLevel -> Input -> [GitOperationStats]
analyseGitOperations lvl rawLines =
    let f = case lvl of
                Hour -> logDateEqHour
                Minute -> logDateEqMinute
        groups = groupBy (f `on` getDate) $ parseLogLines rawLines
    in mapMaybe summarizeGitOperations groups

-- | Return the duration of clone (clone and shallow clone) operations
gitRequestDuration :: Input -> [RequestDurationStat]
gitRequestDuration = flip collectRequestDurations isAuthenticatedGitOp

protocolStatsByHour :: Input -> [ProtocolStats]
protocolStatsByHour rawLines = let  groups = groupBy (logDateEqHour `on` getDate) $ filter f $ parseLogLines rawLines
                                in map protocolStats groups
                            where f line = isOutgoingLogLine line && isGitOperation line

protocolStats :: [LogLine] -> ProtocolStats
protocolStats = foldl' aggregate emptyStats
  where
    aggregate (ProtocolStats date ssh http) logLine =
          let !ssh'   = if isSsh logLine then ssh + 1 else ssh
              !http'  = if isHttp logLine then http + 1 else http
              !date'  = if defaultDate == date then getDate logLine else date
          in ProtocolStats date' ssh' http'
    emptyStats = ProtocolStats defaultDate 0 0
    defaultDate :: LogDate
    defaultDate = def

-- | Return the number of clone operations per repository
repositoryStats :: Input -> [RepositoryStat]
repositoryStats xs =
    let gitOps        = filter (\l -> isGitOperation l && isClone l) $ parseLogLines xs
        perRepo       = groupByRepo $ sortBy (compare `on` repoSlug) gitOps
    in  sortBy (flip compare `on` getNumberOfClones) $ map t perRepo
  where
    groupByRepo      = groupBy ((==) `on` repoSlug)
    repoSlug         = lower . extractRepoSlug . getAction
    lower            = fmap (S.map toLower)
    t []             = StatUnavailable
    t logLines@(x:_) = RepositoryStat (fromMaybe "n/a" (repoSlug x)) (length logLines)



-- =================================================================================



collectRequestDurations :: Input -> (LogLine -> Bool) -> [RequestDurationStat]
collectRequestDurations rawLines p = map m $ filter f $ parseLogLines rawLines
  where
    clientIp line = head $ S.split ',' (getRemoteAdress line)
    ops           = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
    f line        = isOutgoingLogLine line && p line && isGitOperation line
    m line        =  let  duration    = fromMaybe 0 $ getRequestDuration line
                          zero        = replicate 5 0
                          inc op      = if op line then (+duration) else id
                          missOps     = map (inc . isUncachedOperation) ops
                          hitOps      = map (inc . isCachedOperation) ops
                          username'   = fromMaybe "-" $ getUsername line
                          misses      = zipWith id missOps zero
                          hits        = zipWith id hitOps zero
                          rd          = RequestDurationStat (getDate line) (clientIp line) misses hits username'
                     in rd `deepseq` rd



summarizeGitOperations :: [LogLine] -> Maybe GitOperationStats
summarizeGitOperations = foldl' aggregate Nothing . filter isOutgoingLogLine
  where
    zero = replicate 5 0
    aggregate Nothing logLine = acc (getDate logLine) zero zero logLine
    aggregate (Just (GitOperationStats date misses hits)) logLine = acc date misses hits logLine
    acc date' misses hits logLine =
      let ops         = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
          inc op      = if op logLine then (+1) else (+0)
          missOps     = map (inc . isUncachedOperation) ops
          hitOps      = map (inc . isCachedOperation) ops
          misses'     = zipWith id missOps misses
          hits'       = zipWith id hitOps hits
          gos         = Just $ GitOperationStats date' misses' hits'
      in gos `deepseq` gos

-- | Return the repo slug from the logged action.
--
-- E.g. for "GET /scm/CONF/confluence.git/info/refs HTTP/1.1" this would return:
--      "/CONF/confluence.git"
extractRepoSlug :: Action -> Maybe S.ByteString
extractRepoSlug action =
    let elems = S.split '/' (getPath action)
        f     = takeWhile (\s -> s /= "info" && not ("git" `S.isPrefixOf` s)) . dropWhile (`elem` ["", "scm", "git"])
    in Just $ '/' `S.cons` (S.intercalate "/" (f elems))


