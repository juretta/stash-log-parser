{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stash.Log.Common
( logDateEqHour
, logDateEqMinute
, toLocalTime
, isSsh
, isHttp
, isGitOperation
, isPush
, isFetch
, isClone
, isShallowClone
, isRefAdvertisement
, isOutgoingLogLine
, isAuthenticatedGitOp
, isCachedOperation
, isUncachedOperation
) where

import qualified Data.ByteString.Char8 as S
import           Data.Fixed
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Stash.Log.Parser

logDateEqHour :: LogDate -> LogDate -> Bool
logDateEqHour a b = getYear a   == getYear b &&
                getMonth a      == getMonth b &&
                getDay a        == getDay b &&
                getHour a       == getHour b

logDateEqMinute :: LogDate -> LogDate -> Bool
logDateEqMinute a b = getYear a   == getYear b &&
                getMonth a        == getMonth b &&
                getDay a          == getDay b &&
                getHour a         == getHour b &&
                getMinute a       == getMinute b

-- | Convert LogDate to LocalTime
--
-- >>> toLocalTime (LogDate 2014 9 12 23 15 22 300)
-- 2014-09-12 23:15:22
toLocalTime :: LogDate -> LocalTime
toLocalTime LogDate{..} = LocalTime (fromGregorian (fromIntegral getYear) getMonth getDay) (TimeOfDay getHour getMinute (convertFixed getSeconds))
  where
    convertFixed :: Int -> Fixed E12
    convertFixed = (*10^12) . toEnum . fromEnum


-- =================================================================================
--                                Predicates
-- =================================================================================

isAuthenticatedGitOp :: LogLine -> Bool
isAuthenticatedGitOp line = isJust (getUsername line)


-- |
-- >>> let ll = parseLogLine "172.24.20.22,172.24.36.105,127.0.0.1 | https | o0x3357784x7 | foo | 2014-07-16 00:00:01,029 | \"GET /scm/bizplat/omg.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.9.5\" | - | - | - | "
-- >>> fmap isGitOperation ll
-- Just True
isGitOperation :: LogLine -> Bool
isGitOperation line = any (\g -> g line) ops
            where ops = [isClone, isFetch, isShallowClone, isPush, isRefAdvertisement]

-- As of 1.1.2 of the clone cache plugin, refs are explicitly listed in the
-- labels field, most of the data we have does _not_ have that information though
isRefAdvertisement :: LogLine -> Bool
isRefAdvertisement logLine = isAuthenticatedGitOp logLine && isOutgoingLogLine logLine && refAdvertisement logLine
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


inLabel :: S.ByteString -> LogLine -> Bool
inLabel name logLine =  let labels = getLabels logLine
                        in name `elem` labels

isCachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
isCachedOperation op logLine = op logLine && isCacheHit logLine

isUncachedOperation :: (LogLine -> Bool) -> LogLine -> Bool
isUncachedOperation op logLine = op logLine && isCacheMiss logLine

-- | Check whether this is a log line for a response ("outgoing")
isOutgoing :: RequestId -> Bool
isOutgoing rid = getInOrOut rid == 'o'

isOutgoingLogLine :: LogLine -> Bool
isOutgoingLogLine = isOutgoing . getRequestId



-- |
-- >>> let ll = parseLogLine "172.24.20.22,172.24.36.105,127.0.0.1 | https | i0x3357784x7 | - | 2014-07-16 00:00:01,029 | \"GET /scm/bizplat/omg.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.9.5\" | - | - | - | "
-- >>> fmap isSsh ll
-- Just False
isSsh :: LogLine -> Bool
isSsh logLine = getProtocol logLine == "ssh"

-- |
-- >>> let ll = parseLogLine "172.24.20.22,172.24.36.105,127.0.0.1 | https | i0x3357784x7 | - | 2014-07-16 00:00:01,029 | \"GET /scm/bizplat/omg.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.9.5\" | - | - | - | "
-- >>> fmap isHttp ll
-- Just True
isHttp :: LogLine -> Bool
isHttp logLine = proto == "http" || proto == "https"
                where proto = getProtocol logLine
