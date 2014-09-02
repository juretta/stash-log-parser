{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Function         (on)
import           Data.List             (foldl', groupBy, isPrefixOf, sortBy)
import           Data.Maybe            (fromMaybe, isJust)
import qualified Data.String.Utils     as UT
import           Stash.Log.Common      (logDateEqHour)
import           Stash.Log.Parser
import           Text.Printf           (printf)

data GitOperationStats = GitOperationStats {
    getOpStatDate :: !String
  , cacheMisses   :: ![Int] -- clone, fetch, shallow clone, push, ref advertisement
  , cacheHits     :: ![Int]
}

data RequestDurationStat = RequestDurationStat {
    getDurationDate    :: !LogDate
  , getClientIp        :: !String
  , cacheMissDurations :: ![Int]
  , cacheHitDurations  :: ![Int] -- clone, fetch, shallow clone, push, ref advertisement
  , requestUsername    :: !S.ByteString
}

data ProtocolStats = ProtocolStats {
    getProtocolLogDate :: !String
  , getSsh             :: !Int
  , getHttp            :: !Int
}

-- | Parse and aggregate the log file input into a list of hourly GitOperationStats
analyseGitOperations :: Input -> [GitOperationStats]
analyseGitOperations rawLines =
    let formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date)
    in analyseGitOperations' logDateEqHour formatLogDate rawLines

-- | Return the duration of clone (clone and shallow clone) operations
gitRequestDuration :: Input -> [RequestDurationStat]
gitRequestDuration = flip collectRequestDurations authenticatedGitOp

protocolStatsByHour :: Input -> [ProtocolStats]
protocolStatsByHour rawLines = let  groups = groupBy (logDateEqHour `on` getDate) $ filter f $ parseLogLines rawLines
                                    formatLogDate date = printf "%04d-%02d-%02d %02d" (getYear date) (getMonth date) (getDay date) (getHour date)
                                in map (protocolStats formatLogDate) groups
                            where f line = isOutgoingLogLine line && isGitOperation line

protocolStats :: (LogDate -> String) -> [LogLine] -> ProtocolStats
protocolStats formatLogDate = foldl' aggregate (ProtocolStats "" 0 0)
                        where aggregate (ProtocolStats date ssh http) logLine =
                                    let !ssh'   = if isSsh logLine then ssh + 1 else ssh
                                        !http'  = if isHttp logLine then http + 1 else http
                                        !date'  = if null date then formatLogDate $ getDate logLine else date
                                    in ProtocolStats date' ssh' http'

-- | Return the number of clone operations per repository

data RepositoryStat = RepositoryStat {
    getName           :: S.ByteString
  , getNumberOfClones :: Int
} | StatUnavailable deriving (Show)

repositoryStats :: Input -> [RepositoryStat]
repositoryStats xs =
    let gitOps        = filter (\l -> isGitOperation l && isClone l) $ parseLogLines xs
        perRepo       = groupByRepo $ sortBy (compare `on` repoSlug) gitOps
        sortedPerRepo = sortBy (flip compare `on` getNumberOfClones) $ map t perRepo
    in  sortedPerRepo
  where
    groupByRepo      = groupBy ((==) `on` repoSlug)
    repoSlug         = lower . extractRepoSlug . getAction
    lower            = fmap (map toLower)
    t []             = StatUnavailable
    t logLines@(x:_) = RepositoryStat (S.pack $ fromMaybe "n/a" (repoSlug x)) (length logLines)



-- =================================================================================

authenticatedGitOp :: LogLine -> Bool
authenticatedGitOp line = isJust (getUsername line)

collectRequestDurations :: Input -> (LogLine -> Bool) -> [RequestDurationStat]
collectRequestDurations rawLines p = map m $ filter f $ parseLogLines rawLines
        where clientIp line = head $ UT.split "," (S.unpack $ getRemoteAdress line)
              ops           = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
              f line        = isOutgoingLogLine line && p line && isGitOperation line
              m line        =  let  duration    = fromMaybe 0 $ getRequestDuration line
                                    zero        = replicate 5 0
                                    inc op      = if op line then (+duration) else id
                                    missOps     = map (inc . uncachedOperation) ops
                                    hitOps      = map (inc . cachedOperation) ops
                                    username'   = fromMaybe "-" $ getUsername line
                                    !misses     = zipWith id missOps zero
                                    !hits       = zipWith id hitOps zero
                               in RequestDurationStat (getDate line) (clientIp line) misses hits username'

emptyStats :: GitOperationStats
emptyStats = GitOperationStats "" zero zero
            where zero = replicate 5 0

analyseGitOperations' :: (LogDate -> LogDate -> Bool) -> (LogDate -> String) -> Input -> [GitOperationStats]
analyseGitOperations' comp formatLogDate rawLines =
    let groups = groupBy (comp `on` getDate) $ parseLogLines rawLines
    in map (summarizeGitOperations formatLogDate) groups


summarizeGitOperations :: (LogDate -> String) -> [LogLine] -> GitOperationStats
summarizeGitOperations formatLogDate = foldl' aggregate emptyStats . filter isOutgoingLogLine
                        where aggregate (GitOperationStats date misses hits) logLine =
                                let ops         = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]
                                    inc op      = if op logLine then (+1) else (+0)
                                    missOps     = map (inc . uncachedOperation) ops
                                    hitOps      = map (inc . cachedOperation) ops
                                    date'       = if null date then formatLogDate $ getDate logLine else date
                                    misses'     = zipWith id missOps misses
                                    hits'       = zipWith id hitOps hits
                                in GitOperationStats date' misses' hits'

-- | Return the repo slug from the logged action.
--
-- E.g. for "GET /scm/CONF/confluence.git/info/refs HTTP/1.1" this would return:
--      "/CONF/confluence.git"
extractRepoSlug :: Action -> Maybe String
extractRepoSlug action =
    let elems = UT.split ("/" :: String) (S.unpack $ getPath action)
        f     = takeWhile (\s -> s /= "info" && not ("git" `isPrefixOf` s)) . dropWhile (`elem` ["", "scm", "git"])
    in Just $ '/' : UT.join "/" (f elems)

-- =================================================================================
--                                Predicates
-- =================================================================================

isGitOperation :: LogLine -> Bool
isGitOperation line = any (\g -> g line) ops
            where ops = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]

-- As of 1.1.2 of the clone cache plugin, refs are explicitly listed in the
-- labels field, most of the data we have does _not_ have that information though
isRefAdvertisement :: LogLine -> Bool
isRefAdvertisement logLine = authenticatedGitOp logLine && isOutgoingLogLine logLine && refAdvertisement logLine
            where
                action      = getAction logLine
                path        = getPath action
                method      = getMethod action
                refAdvertisement line
                            | isSsh line        = isRefs line || not (any ($ line) [isClone, isFetch, isShallowClone])
                            | isHttp line       = ".git/info/refs" `S.isSuffixOf` path && "GET" == method
                            | otherwise         = False

isCacheHit, isCacheMiss, isFetch, isClone, isShallowClone, isPush, isRefs, isShallow :: LogLine -> Bool
isCacheHit = inLabel "cache:hit"

isCacheMiss = not . inLabel "cache:hit" -- treat as cache miss if the cache:* label is missing

isFetch logLine = inLabel "fetch" logLine && not (isClone logLine || isShallowClone logLine)

isClone line = inLabel "clone" line && not (isShallow line)

isShallowClone logLine = inLabel "shallow clone" logLine || (
            inLabel "clone" logLine && isShallow logLine)

isPush logLine = inLabel "push" logLine || isPushAction (getAction logLine)
    where isPushAction (HttpAction method path) =
                    ".git/git-receive-pack" `S.isSuffixOf` path && "POST" == method
          isPushAction (SshAction method _)  =
                    "git-receive-pack" `S.isInfixOf` method

isRefs = inLabel "refs"

isShallow = inLabel "shallow"

isSsh :: LogLine -> Bool
isSsh logLine = getProtocol logLine == "ssh"

isHttp :: LogLine -> Bool
isHttp logLine = proto == "http" || proto == "https"
                where proto = getProtocol logLine



inLabel :: String -> LogLine -> Bool
inLabel name logLine =  let labels = getLabels logLine
                        in name `elem` labels

cachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
cachedOperation op logLine = op logLine && isCacheHit logLine

uncachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
uncachedOperation op logLine = op logLine && isCacheMiss logLine

-- | Check whether this is a log line for a response ("outgoing")
isOutgoing :: RequestId -> Bool
isOutgoing rid = getInOrOut rid == 'o'

isOutgoingLogLine :: LogLine -> Bool
isOutgoingLogLine = isOutgoing . getRequestId


