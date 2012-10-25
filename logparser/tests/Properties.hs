{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the 'Data.Hashable' module.  We test functions by
-- comparing the C and Haskell implementations.

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Test.HUnit as H
import Stash.Log.Analyser
import Stash.Log.Parser
import Data.Maybe
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
    where inputLine = "63.246.22.196,172.16.3.45 | https | i1112x6x32 | ssaasen | 2012-08-22 18:32:08,505 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | shallow clone | - | - | "

parsedLogLine2 = parseLogLine inputLine
    where inputLine = "63.246.22.196,172.16.3.45 | https | i1112x6x32 | - | 2012-08-22 18:32:08,505 | \"GET /git/ATLASSIAN/jira.git/info/refs HTTP/1.1\" | \"\" \"git/1.7.4.1\" | fetch | - | - | "

parsedLogLine3 = parseLogLine inputLine
    where inputLine = "172.16.3.7 | ssh | o949x7523178x1 | atlaseye_user | 2012-10-23 15:49:46,461 | git-upload-pack '/CONF/teamcal.git' | - | fetch | 117 | de84i5 | "

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

test_logLineParseAction = H.assertEqual
    "Should parse the action correctly for http"
    "/git/ATLASSIAN/jira.git/info/refs"
    (getPath $ getAction $ fromJust parsedLogLine)

test_logLineParseActionSsh = H.assertEqual
    "Should parse the action correctly for ssh"
    "/CONF/teamcal.git"
    (getPath $ getAction $ fromJust parsedLogLine3)

test_logLineParseDetails = H.assertEqual
    "Should parse the labels correctly"
    "\"\" \"git/1.7.4.1\""
    (getDetails $ fromJust parsedLogLine)

test_logLineParseLabels = H.assertEqual
    "Should parse the labels correctly"
    ["shallow clone"]
    (getLabels $ fromJust parsedLogLine)

test_logLineParseRequestId = H.assertEqual
    "Should parse the request id"
    'i'
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


test_plotDataConcurrentConn = H.assertEqual
    "Should aggregate the max connection per minute"
    [
      (DateValuePair (LogDate 2012 8 22 18 32 55 300) 10),
      (DateValuePair (LogDate 2012 8 22 20 28 04 864) 1),
      (DateValuePair (LogDate 2012 8 22 21 7 11 798) 4),
      (DateValuePair (LogDate 2012 8 22 23 59 59 460) 5),
      (DateValuePair (LogDate 2012 8 23 17 44 20 123) 4),
      (DateValuePair (LogDate 2012 8 23 17 48 20 505) 2)
    ]
    (plotDataConcurrentConnMinute dataLogLines)

test_plotDataConcurrentConnHour = H.assertEqual
    "Should aggregate the max connection per hour"
    [
      (DateValuePair (LogDate 2012 8 22 18 32 55 300) 10),
      (DateValuePair (LogDate 2012 8 22 20 28 04 864) 1),
      (DateValuePair (LogDate 2012 8 22 21 7 11 798) 4),
      (DateValuePair (LogDate 2012 8 22 23 59 59 460) 5),
      (DateValuePair (LogDate 2012 8 23 17 48 20 505) 4)
    ]
    (plotDataConcurrentConnHour dataLogLines)

-- | Test maxConcurrent
test_maxConcurrent = H.assertEqual
    "Should return the correct number of concurrent requests"
    10
    (maxConcurrent dataLogLines)

test_protocolCount = H.assertEqual
    "Should count the protocol correctly"
    [("https", 6), ("ssh", 2)]
    (protocolCount dataLogLines)

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "analyser"
      [ --testProperty "analyser/countLines" prop_countLines
        testCase "analyser/maxConcurrent" test_maxConcurrent
        ,testCase "analyser/protocolCount" test_protocolCount
        ,testCase "analyser/dataConcurrentConn logDateEqMin" test_plotDataConcurrentConn
        ,testCase "analyser/dataConcurrentConn logDateEqHour" test_plotDataConcurrentConnHour
      ],
      testGroup "parser"
      [ testCase "parser/parse empty String" test_logLineParserEmpty
        ,testCase "parser/parse single line" test_logLineParseSingleLine
        ,testCase "parser/parse protocol (https)" test_logLineParseProtocol
        ,testCase "parser/parse protocol (ssh)" test_logLineParseProtocolSsh
        ,testCase "parser/parse request id" test_logLineParseRequestId
        ,testCase "parser/parse action" test_logLineParseAction
        ,testCase "parser/parse details" test_logLineParseDetails
        ,testCase "parser/parse labels" test_logLineParseLabels
        ,testCase "parser/parse request counter" test_logLineParseRequestIdCounter
        ,testCase "parser/parse request concurrent requests" test_logLineParseRequestIdConcurrent
        ,testCase "parser/parse log entry date" test_parseLogEntryDate
        ,testCase "parser/parse username (Just)" test_logLineParseUsernameAsJust
        ,testCase "parser/parse username (Nothing)" test_logLineParseUsernameAsNothing
      ]
    ]