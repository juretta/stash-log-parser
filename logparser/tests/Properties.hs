{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Test.HUnit as H
import Stash.Log.Analyser
import Stash.Log.Parser
import Stash.Log.GitOpsAnalyser
import Stash.Log.Input
import Data.Maybe
import Data.List                                    (sort)
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit


instance Arbitrary L.ByteString where
    arbitrary   = fmap L.pack arbitrary

------------------------------------------------------------------------
-- * Properties


------------------------------------------------------------------------

test_logLineParserEmpty = H.assertEqual
  "Should get Nothing from an empty string"
  Nothing
  ( parseLogLine "" )

parsedLogLine = parseLogLine inputLine
    where inputLine = "63.246.22.196,172.16.3.45 | https | o1112x6x32 | ssaasen | 2012-08-22 18:32:08,505 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | shallow clone | - | - | "

parsedLogLine2 = parseLogLine inputLine
    where inputLine = "63.246.22.196,172.16.3.45 | https | i1112x6x32 | - | 2012-08-22 18:32:08,505 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | fetch | - | - | "

parsedLogLine3 = parseLogLine inputLine
    where inputLine = "172.16.3.7 | ssh | o949x7523178x1 | atlaseye_user | 2012-10-23 15:49:46,461 | git-upload-pack '/CONF/teamcal.git' | - | fetch | 117 | de84i5 | "

parsedLogLine4 = parseLogLine inputLine
    where inputLine = "63.246.22.41,172.16.1.187 | https | i0x4771443x6 | - | 2012-12-10 00:00:00,199 | \"GET /git/STASH/stash.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | - | - | - | "

parsedLogLine5 = parseLogLine inputLine
    where inputLine = "172.16.3.7 | ssh | o357x407998x2 | atlaseye_user | 2013-03-05 05:57:20,505 | SSH - git-upload-pack '/CONF/teamcal.git' | - | clone | 145 | ofq0l6 | "

parsedLogLine6 = parseLogLine inputLine
    where inputLine = "172.26.24.201,127.0.0.1 | https | o1167x35420x10 | klaus.tester | 2013-06-13 19:27:27,302 \
    \| \"POST /scm/testlab/point-jmeter.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.8.2\" | clone, cache:hit | 3499 | 8wovkm | "

parsedLogLine7 = parseLogLine inputLine
    where inputLine = "172.26.24.201,127.0.0.1 | https | o1167x35420x10 | klaus.tester | 2013-06-13 19:27:27,302 \
    \| \"POST /scm/testlab/point-jmeter.git/git-receive-pack HTTP/1.1\" | \"\" \"git/1.7.8.2\" | - | 3499 | 8wovkm | "

parsedLogLine8 = parseLogLine inputLine
    where inputLine = "172.16.3.7 | ssh | o357x407998x2 | atlaseye_user | 2013-03-05 05:57:20,505 | SSH - git-receive-pack '/CONF/teamcal.git' | - | push | 145 | ofq0l6 | "

parsedLogLine9 = parseLogLine inputLine
    where inputLine = "172.16.3.7 | ssh | o357x407998x2 | atlaseye_user | 2013-03-05 05:57:20,505 | SSH - git-receive-pack '/CONF/teamcal.git' | - | - | 145 | ofq0l6 | "

parsedLogLineShallowNegotiation = parseLogLine inputLine
    where inputLine = "23.23.30.99,172.24.4.5,127.0.0.1 | https | o1439x1423920x69 | bamboo_user | 2013-08-22 23:59:57,763 | \"POST /scm/JIRA/servicedesk.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.9.5\" | shallow negotiation, cache:hit | 70 | q09lhc |"

parsedLogLineShallowCloneLabelNew = parseLogLine inputLine
    where inputLine = "23.23.30.99,172.24.4.5,127.0.0.1 | https | o1439x1423920x69 | bamboo_user | 2013-08-22 23:59:57,763 | \"POST /scm/JIRA/servicedesk.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.9.5\" | shallow, clone, cache:hit | 70 | q09lhc | "

parsedLogLineShallowCloneLabelOld = parseLogLine inputLine
    where inputLine = "23.23.30.99,172.24.4.5,127.0.0.1 | https | o1439x1423920x69 | bamboo_user | 2013-08-22 23:59:57,763 | \"POST /scm/JIRA/servicedesk.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.9.5\" | shallow clone, cache:hit | 70 | q09lhc | "


test_parseLogEntryDate = H.assertEqual
    "Should parse the date correctly"
    (LogDate 2012 8 22 18 32 08 505)
    (getDate $ fromJust parsedLogLine)

test_logLineParseSingleLine = H.assertBool
    "Should parse a valid log line into a LogLine"
    (isJust parsedLogLine)

test_logLineParseProtocol = H.assertEqual
    "Should parse the protocol correctly"
    "https"
    (getProtocol $ fromJust parsedLogLine)

test_logLineParseProtocolSsh = H.assertEqual
    "Should parse the protocol correctly"
    "ssh"
    (getProtocol $ fromJust parsedLogLine3)

test_logLineParseProtocolSsh2 = H.assertEqual
    "Should parse the protocol correctly"
    "ssh"
    (getProtocol $ fromJust parsedLogLine5)

test_logLineParseAction = H.assertEqual
    "Should parse the action correctly for http"
    "/git/ATLASSIAN/jira.git/info/refs"
    (getPath $ getAction $ fromJust parsedLogLine)

test_logLineParseActionSsh = H.assertEqual
    "Should parse the action correctly for ssh"
    "/CONF/teamcal.git"
    (getPath $ getAction $ fromJust parsedLogLine3)

test_extractActionSsh = H.assertEqual
    "Should parse the action correctly for ssh"
    (Just "/CONF/teamcal.git")
    (extractRepoSlug $ getAction $ fromJust parsedLogLine3)

test_extractActionHTTP = H.assertEqual
    "Should parse the action correctly for http"
    (Just "/ATLASSIAN/jira.git")
    (extractRepoSlug $ getAction $ fromJust parsedLogLine)

test_extractActionHTTPUploadPack = H.assertEqual
    "Should parse the action correctly for an upload-pack operation via http"
    (Just "/testlab/point-jmeter.git")
    (extractRepoSlug $ getAction $ fromJust parsedLogLine6)

test_classifyRefAdv = H.assertBool
    "Should identify ref advertisement"
    (isRefAdvertisement $ fromJust parsedLogLine)

test_classifyHttpPush = H.assertBool
    "POST to git-receive-pack is a push even with no label present"
    (isPush $ fromJust parsedLogLine7)

test_classifySshPush = H.assertBool
    "SSH using git-receive-pack on the remote end is a push even with no label present"
    (isPush $ fromJust parsedLogLine9)

test_classifyPushWithLabel = H.assertBool
    "Classify push with label"
    (isPush $ fromJust parsedLogLine8)

test_logLineParseDetails = H.assertEqual
    "Should parse the labels correctly"
    "\"\" \"git/1.7.4.1\""
    (getDetails $ fromJust parsedLogLine)

test_logLineParseLabels = H.assertEqual
    "Should parse the labels correctly"
    ["shallow clone"]
    (getLabels $ fromJust parsedLogLine)

test_logLineParseLabelsChangedLogFormat = H.assertEqual
    "Should parse the labels correctly"
    ["clone"]
    (getLabels $ fromJust parsedLogLine5)

test_logLineParseLabelsShallowCloneOld = H.assertBool
    "Should parse the old 'shallow clone' label correctly"
    (isShallowClone $ fromJust parsedLogLineShallowCloneLabelOld)

test_logLineParseLabelsShallowCloneNew = H.assertBool
    "Should parse the new 'shallow, clone' labels correctly"
    (isShallowClone $ fromJust parsedLogLineShallowCloneLabelNew)

test_logLineParseLabelsCloneOld = H.assertBool
    "Should parse the old 'shallow clone' label correctly"
    (not $ isClone $ fromJust parsedLogLineShallowCloneLabelOld)

test_logLineParseLabelsCloneNew = H.assertBool
    "Should parse the new 'shallow, clone' labels correctly"
    (not $ isClone $ fromJust parsedLogLineShallowCloneLabelNew)

test_logLineParseDuration = H.assertEqual
    "Should parse the duration correctly"
    (Just 117)
    (getRequestDuration $ fromJust parsedLogLine3)

test_logLineParseDurationNothing = H.assertEqual
    "Should parse the duration correctly"
    Nothing
    (getRequestDuration $ fromJust parsedLogLine4)

test_logLineParseRequestId = H.assertEqual
    "Should parse the request id"
    'o'
    (getInOrOut $ getRequestId $ fromJust parsedLogLine)

test_logLineParseRequestIdCounter = H.assertEqual
    "Should parse the request id -> request counter"
    6
    (getRequestCounter $ getRequestId $ fromJust parsedLogLine)

test_logLineParseRequestIdConcurrent = H.assertEqual
    "Should parse the request id -> concurrent requests"
    32
    (getConcurrentRequests $ getRequestId $ fromJust parsedLogLine)

test_logLineParseUsernameAsJust = H.assertEqual
    "Should parse a username"
    (Just "ssaasen")
    (getUsername $ fromJust parsedLogLine)

test_logLineParseUsernameAsNothing = H.assertEqual
    "Should parse a username"
    Nothing
    (getUsername $ fromJust parsedLogLine2)

------------------------------------------------------------------------
-- Analyser
dataLogLines :: [L.ByteString]
dataLogLines = [
    "63.246.22.196,172.16.3.45 | https | i1112x6x6 | - | 2012-08-22 18:32:08,505 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | - | - | - | "
    ,"63.246.22.196,172.16.3.45 | https | i1112x10x10 | - | 2012-08-22 18:32:55,300 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | - | - | - | "
    ,"172.16.3.7 | ssh | o949x7523178x1 | atlaseye_user | 2012-08-22 20:28:04,864 | git-upload-pack '/CONF/teamcal.git' | - | fetch | 117 | de84i5 | "
    ,"63.246.22.222,172.16.3.45 | https | o1266x9120x4 | bamboo_user | 2012-08-22 21:07:11,798 | \"POST /git/STASH/stash.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.11.1\" | clone | 12426 | - | "
    ,"63.246.22.222,172.16.3.45 | https | o1439x17982x5 | bamboo_user | 2012-08-22 23:59:59,296 | \"POST /git/ATLASSIAN/jira.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.11.1\" | shallow clone | 289 | 1h32yz6 | "
    ,"63.246.22.222,172.16.3.45 | https | o1439x17983x4 | bamboo_user | 2012-08-22 23:59:59,460 | \"POST /git/ATLASSIAN/jira.git/git-upload-pack HTTP/1.1\" | \"\" \"git/1.7.11.1\" | shallow clone | 368 | 1p7ya9s | "
    ,"63.246.22.222,172.16.3.45 | https | i2112x2x4 | - | 2012-08-23 17:44:20,123 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"JGit/unknown\" | - | - | - | "
    ,"63.246.22.196,172.16.3.45 | ssh | i2112x4x2 | - | 2012-08-23 17:48:20,505 | git-upload-pack '/CONF/teamcal.git' | \"\" \"git/1.7.4.1\" | - | - | - | "]

protocalStatsLogLines :: [L.ByteString]
protocalStatsLogLines = [
   "151.193.220.132 | ssh | o0x341798x1 | SG0922458 | 2013-07-09 00:00:34,655 | SSH - git-upload-pack '/hd/raspberryteam.git' | - | - | - | gqkf8q | "
  ,"151.193.220.129 | http | o0x341799x2 | - | 2013-07-09 00:00:34,758 | \"GET /login HTTP/1.1\" | \"\" \"null\" | - | - | - | "
 , "10.136.130.153,151.193.220.131 | http | o163x349445x1 | SG0210396 | 2013-07-09 00:00:38,817 | \"GET /login HTTP/1.1\" | \"\" \"null\" | - | 46 | 1oaiaol | " 
  ,"151.193.220.132 | ssh | o0x341798x1 | SG0922458 | 2013-07-09 00:00:34,973 | SSH - git-upload-pack '/hd/raspberryteam.git' | - | clone | 318 | gqkf8q | "
  , "10.136.130.153,151.193.220.131 | http | i163x349445x1 | SG0210396 | 2013-07-09 00:00:35,817 | \"GET /scm/tc/tripcase-rails.git/info/refs HTTP/1.1\" | \"\" \"git/1.8.3.2\" | - | 46 | 1oaiaol | "
  , "10.136.130.153,151.193.220.131 | http | o163x349445x1 | SG0210396 | 2013-07-09 00:00:36,817 | \"GET /scm/tc/tripcase-rails.git/info/refs HTTP/1.1\" | \"\" \"git/1.8.3.2\" | - | 46 | 1oaiaol | "
  , "10.136.130.153,151.193.220.131 | http | o163x349445x1 | SG0210396 | 2013-07-09 00:00:37,817 | \"GET /scm/tc/tripcase-rails.git/info/refs HTTP/1.1\" | \"\" \"git/1.8.3.2\" | - | 46 | 1oaiaol | "
  , "10.136.130.153,151.193.220.131 | http | o163x349445x1 | SG0210396 | 2013-07-09 00:00:38,817 | \"GET /scm/tc/tripcase-rails.git/info/refs HTTP/1.1\" | \"\" \"git/1.8.3.2\" | - | 46 | 1oaiaol | " ]



test_concurrentConnections = H.assertEqual
    "Should aggregate the max connection per hour"
    [
      (DateValuePair (LogDate 2012 8 22 18 32 55 300) 10),
      (DateValuePair (LogDate 2012 8 22 20 28 04 864) 1),
      (DateValuePair (LogDate 2012 8 22 21 7 11 798) 4),
      (DateValuePair (LogDate 2012 8 22 23 59 59 460) 5),
      (DateValuePair (LogDate 2012 8 23 17 48 20 505) 4)
    ]
    (concurrentConnections dataLogLines)


test_protocolCount = H.assertEqual
    "Should count the protocol correctly"
    [(1,0),(0,1),(0,2)]
    (fmap (\(ProtocolStats _ ssh http) -> (ssh, http)) $ protocolStatsByHour dataLogLines)

test_protocolCountMixedRequests = H.assertEqual
    "Should count the protocol correctly in the presence of non-hosting HTTP requests"
    [(2, 3)]
    (fmap (\(ProtocolStats _ ssh http) -> (ssh, http)) $ protocolStatsByHour protocalStatsLogLines)

test_identifyRefAdvertisement_SSH = H.assertEqual
    "Should identify an SSH based ref advertisement correctly"
    True
    (isRefAdvertisement $ fromJust $ parseLogLine "172.16.1.187 | ssh | o912x7392530x2 | ssaasen | 2012-12-14 15:12:20,602 | git-upload-pack '/CONF/confluence.git' | - | - | 4284 | jfvu89 |  ")

test_identifyRefAdvertisement_SSHClone = H.assertEqual
    "Should not identify clone response as ref advertisement"
    False
    (isRefAdvertisement $ fromJust $ parseLogLine "172.16.1.187 | ssh | o912x7392530x2 | ssaasen | 2012-12-14 15:12:20,602 | git-upload-pack '/CONF/confluence.git' | - | clone | 4284 | jfvu89 |  ")

test_identifyRefAdvertisement_SSHIncoming = H.assertEqual
    "Should not identify incoming request as ref advertisement"
    False
    (isRefAdvertisement $ fromJust $ parseLogLine "172.16.1.187 | ssh | i912x7392530x2 | ssaasen | 2012-12-14 15:12:20,602 | git-upload-pack '/CONF/confluence.git' | - | - | 4284 | jfvu89 |  ")

test_identifyRefAdvertisement_SSHAuthenticated = H.assertEqual
    "Should not identify unauthenticated request as ref advertisement"
    False
    (isRefAdvertisement $ fromJust $ parseLogLine "172.16.1.187 | ssh | o912x7392530x2 | - | 2012-12-14 15:12:20,602 | git-upload-pack '/CONF/confluence.git' | - | - | 4284 | jfvu89 |  ")

test_identifyRefAdvertisement_HttpAction = H.assertEqual
    "Should identify an HTTP based ref advertisement correctly"
    True
    (isRefAdvertisement $ fromJust $ parseLogLine "63.246.22.222,172.16.3.45 | https | o2112x2x4 | ssaasen | 2012-08-23 17:44:20,123 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"JGit/unknown\" | - | - | - | ")

test_identifyRefAdvertisement_HttpLabel = H.assertEqual
    "Should identify an HTTP based ref advertisement correctly"
    True
    (isRefAdvertisement $ fromJust $ parseLogLine "63.246.22.222,172.16.3.45 | https | o2112x2x4 | ssaasen | 2012-08-23 17:44:20,123 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"JGit/unknown\" | refs | - | - | ")

------------------------------------------------------------------------
test_sortFilesAsc = H.assertEqual
    "Should sort the given files correctly"
    ["atlassian-stash-access-2012-11-29.1.log.bz2",
     "atlassian-stash-access-2012-11-29.2.log.bz2",
     "atlassian-stash-access-2012-11-29.4.log.bz2",
     "atlassian-stash-access-2012-11-29.10.log.bz2",
     "atlassian-stash-access.log"]
     (sortLogFiles input)
    where input = ["atlassian-stash-access-2012-11-29.4.log.bz2",
                     "atlassian-stash-access-2012-11-29.1.log.bz2",
                     "atlassian-stash-access-2012-11-29.10.log.bz2",
                     "atlassian-stash-access.log",
                     "atlassian-stash-access-2012-11-29.2.log.bz2"]

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "analyser"
      [ --testProperty "analyser/countLines" prop_countLines
         testCase "analyser/protocolCount" test_protocolCount
        ,testCase "analyser/protocolCountMixedRequests" test_protocolCountMixedRequests
        ,testCase "analyser/dataConcurrentConn logDateEqHour" test_concurrentConnections
        ,testCase "analyser/isRefAdvertisement ssh" test_identifyRefAdvertisement_SSH
        ,testCase "analyser/isRefAdvertisement ignore incoming ssh" test_identifyRefAdvertisement_SSHIncoming
        ,testCase "analyser/isRefAdvertisement require authenticated ssh" test_identifyRefAdvertisement_SSHAuthenticated
        ,testCase "analyser/isRefAdvertisement ignore clones ssh" test_identifyRefAdvertisement_SSHClone
        ,testCase "analyser/isRefAdvertisement http action" test_identifyRefAdvertisement_HttpAction
        ,testCase "analyser/isRefAdvertisement http label" test_identifyRefAdvertisement_HttpLabel
        ,testCase "analyser/isShallowClone (shallow clone)" test_logLineParseLabelsShallowCloneOld
        ,testCase "analyser/isShallowClone (shallow, clone)" test_logLineParseLabelsShallowCloneNew
        ,testCase "analyser/not isClone (shallow clone)" test_logLineParseLabelsCloneNew
        ,testCase "analyser/not isClone (shallow, clone)" test_logLineParseLabelsCloneOld
      ],
      testGroup "Files"
      [
        testCase "files/sortLogFiles" test_sortFilesAsc
      ],
      testGroup "parser"
      [ testCase "parser/parse empty String" test_logLineParserEmpty
        ,testCase "parser/parse single line" test_logLineParseSingleLine
        ,testCase "parser/parse classify log line" test_classifyRefAdv
        ,testCase "parser/parse classify log line - HTTP push" test_classifyHttpPush
        ,testCase "parser/parse classify log line - SSH push" test_classifySshPush
        ,testCase "parser/parse classify log line - push label" test_classifyPushWithLabel
        ,testCase "parser/parse protocol (https)" test_logLineParseProtocol
        ,testCase "parser/parse protocol (ssh)" test_logLineParseProtocolSsh
        ,testCase "parser/parse protocol (ssh - changed log format)" test_logLineParseProtocolSsh2
        ,testCase "parser/parse request id" test_logLineParseRequestId
        ,testCase "parser/parse duration" test_logLineParseDuration
        ,testCase "parser/parse duration - Nothing" test_logLineParseDurationNothing
        ,testCase "parser/parse action" test_logLineParseAction
        ,testCase "parser/parse details" test_logLineParseDetails
        ,testCase "parser/parse labels" test_logLineParseLabels
        ,testCase "parser/parse labels (changed log format)" test_logLineParseLabelsChangedLogFormat
        ,testCase "parser/parse request counter" test_logLineParseRequestIdCounter
        ,testCase "parser/parse request concurrent requests" test_logLineParseRequestIdConcurrent
        ,testCase "parser/parse log entry date" test_parseLogEntryDate
        ,testCase "parser/parse username (Just)" test_logLineParseUsernameAsJust
        ,testCase "parser/parse username (Nothing)" test_logLineParseUsernameAsNothing
        ,testCase "parser/extract Action HTTP" test_extractActionHTTP
        ,testCase "parser/extract Action HTTP (upload-pack)" test_extractActionHTTPUploadPack
        ,testCase "parser/extract Action SSH" test_extractActionSsh
      ]
    ]
