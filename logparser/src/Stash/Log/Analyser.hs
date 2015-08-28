{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stash.Log.Analyser
( countLines
, countRequestLines
, Input
, DateValuePair(..)
, RequestClassification(..)
, concurrentConnections
, showLines
, classifyRequests
) where

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (foldl')
import           Stash.Log.Common
import           Stash.Log.Parser

data DateValuePair = DateValuePair {
     getLogDate :: !LogDate
    ,getValue   :: !Integer
} deriving (Show, Eq)


{-
    Git HTTP
    Git SSH
    Web UI
    FileServer
    REST
-}

data RequestClassification = RequestClassification {
    gitHttp    :: !Integer
  , gitSsh     :: !Integer
  , webUi      :: !Integer
  , fileServer :: !Integer
  , rest       :: !Integer
} deriving (Eq, Show)

instance Monoid RequestClassification where
    mempty = RequestClassification 0 0 0 0 0
    mappend r1 r2 = r1 {
            gitHttp    = gitHttp r1 + gitHttp r2
          , gitSsh     = gitSsh r1 + gitSsh r2
          , webUi      = webUi r1 + webUi r2
          , fileServer = fileServer r1 + fileServer r2
          , rest       = rest r1 + rest r2
        }

-- | Count the number of lines in the given input file
countLines :: L.ByteString -> Integer
countLines input = toInteger $ L.count '\n' input

-- | Count the number of lines marked as incoming (aka request)
countRequestLines :: Input -> Integer
countRequestLines = countLinesWith (\x acc -> if isIncomingLine x then succ acc else acc)


classifyRequests :: Input -> RequestClassification
classifyRequests input = let lines' = filter isOutgoingLine $ parseLogLines input
                         in foldl' (\req@RequestClassification{..} line -> req `mappend` classifyRequest line) mempty lines'

-- |
--
-- >>> let ll = parseLogLine "59.167.133.100,172.24.36.105,127.0.0.1 | https | o1439x3357686x5 | pcherukuri | 2014-07-16 00:00:00,824 | \"GET /rest/api/latest/projects/JIRA/repos/servicedesk/pull-requests/1047/merge HTTP/1.1\" | \"https://stash.atlassian.com/projects/JIRA/repos/servicedesk/pull-requests/1047/overview\" \"Mozilla/5.0 Gecko/20100101 Firefox/30.0\" | - | 9291 | 1hpgdoj | "
-- >>> fmap classifyRequest ll
-- Just (RequestClassification {gitHttp = 0, gitSsh = 0, webUi = 0, fileServer = 0, rest = 1})
--
-- >>> let ll = parseLogLine "172.24.20.22,172.24.36.105,127.0.0.1 | https | o1439x3357686x5 | pcherukuri | 2014-07-16 00:00:01,029 | \"GET /scm/bizplat/omg.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.9.5\" | - | - | - | "
-- >>> fmap classifyRequest ll
-- Just (RequestClassification {gitHttp = 1, gitSsh = 0, webUi = 0, fileServer = 0, rest = 0})
--
-- >>> let ll = parseLogLine "459.167.133.100,172.24.36.105,127.0.0.1 | https | o0x3357876x28 | - | 2014-07-16 00:00:06,796 | \"GET /s/d41d8cd98f00b204e9800998ecf8427e-CDN/en_US/faa46f8/1/38109b1dab5596a818b1e6fb632b3b3c/_/download/contextbatch/css/stash.page.login/batch.css HTTP/1.1\" | \"https://stash.atlassian.com/login?next=/projects\" \"Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0\" | - | 5 | - | "
-- >>> fmap classifyRequest ll
-- Just (RequestClassification {gitHttp = 0, gitSsh = 0, webUi = 0, fileServer = 1, rest = 0})
--
-- >>> let ll = parseLogLine "72.24.36.105 | ssh | o0x3357782x9 | cdoble | 2014-07-16 00:00:00,837 | SSH - git-upload-pack '/jira/workflow-designer.git' | - | ssh:id:463, ssh:label:'JBIAC Workflow Designer' | 5 | 1ny47lx | "
-- >>> fmap classifyRequest ll
-- Just (RequestClassification {gitHttp = 0, gitSsh = 1, webUi = 0, fileServer = 0, rest = 0})
--
-- >>> let ll = parseLogLine "172.24.12.173,172.24.36.105,127.0.0.1 | https | o0x3357873x28 | - | 2014-07-16 00:00:06,673 | \"GET /plugins/servlet/streams HTTP/1.1\" | \"\" \"Jakarta Commons-HttpClient/3.0.1\"  | - | 4 | 1tk57wc | "
-- >>> fmap classifyRequest ll
-- Just (RequestClassification {gitHttp = 0, gitSsh = 0, webUi = 1, fileServer = 0, rest = 0})
classifyRequest :: LogLine -> RequestClassification
classifyRequest line =
     let path = getPath $ getAction line
         isSsh'       = isGitOperation line && isSsh line
         isHttp'      = isGitOperation line && isHttp line
         isFileServer = S.isPrefixOf "/s/" path
         isRest       = S.isPrefixOf "/rest/" path
         isWebUi      = isHttp line && not isFileServer && not isRest && not (isGitOperation line)
    in mempty {
            gitHttp    = if isHttp' then 1 else 0
          , gitSsh     = if isSsh' then 1 else 0
          , webUi      = if isWebUi then 1 else 0
          , fileServer = if isFileServer then 1 else 0
          , rest       = if isRest then 1 else 0
    }

-- The concurrent connection data needs to be aggregated.
-- Example
--
-- 2012-08-22 18:32:08,505 6
-- 2012-08-22 18:32:08,505 10
-- 2012-08-22 18:32:08,505 4
-- ...
-- Should be aggregated on a minute/hour level with the max num per unit
concurrentConnections :: Input -> [DateValuePair]
concurrentConnections inxs = reverse $ uncurry (++) res
        where
            eqf           = logDateEqHour
            f acc logLine =
                    let conn = getConcurrentRequests $ getRequestId logLine
                        dateTime = getDate logLine
                    in case acc of
                     ([prev], xs)-> dateTime `seq` if eqf (getLogDate prev) dateTime
                                 then ([DateValuePair dateTime (max conn (getValue prev))], xs)
                                 else ([DateValuePair dateTime conn], prev : xs)
                     ([], xs)    -> ([DateValuePair dateTime conn], xs)
                     (_, _)      -> ([], [])
            res = foldl' f ([],[]) $ parseLogLines inxs



countLinesWith :: (Num a) => (LogLine -> a -> a) -> Input -> a
countLinesWith f = foldl' count' 0
    where count' acc l = maybe acc (`f` acc) $ parseLogLine l

showLines :: Input -> [Maybe LogLine]
showLines = take 5 . map parseLogLine


-- | Check whether this is a log line for the request ("incoming")
isIncomingLine :: LogLine -> Bool
isIncomingLine line = getInOrOut (getRequestId line) == 'i'

-- | Check whether this is a log line for the response ("outgoing")
isOutgoingLine :: LogLine -> Bool
isOutgoingLine line = getInOrOut (getRequestId line) == 'o'
