{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Input
( sortLogFiles
, readFiles
, FileInfo(..)
, extractFileInfo
, isFileNewer
, filterLastDay
, dropUntilDate
, readLogFiles
, RunConfig(..)
, newRunConfig
) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Codec.Compression.BZip as BZip
import qualified Data.Map as M
import Data.Monoid (mappend)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.List (isSuffixOf, sortBy, groupBy)
import Data.String.Utils (split)
import System.Path.NameManip
import Control.Monad (liftM, liftM2)
import Data.Aeson (decode)
import Data.Time.Clock
import Data.Time.Calendar
import Debug.Trace

data FileInfo = FileInfo {
     year       :: String
    ,month      :: String
    ,day        :: String
    ,counter    :: Int
} deriving (Show, Ord, Eq)

newtype FileDateInfo = FileDateInfo FileInfo

-- | Ignore the counter for equality checks
instance Eq FileDateInfo where
  (FileDateInfo (FileInfo year1 month1 day1 _)) == (FileDateInfo (FileInfo year2 month2 day2 _))
                            = year1 == year2 && month1 == month2 && day1 == day2

type Date = String

-- | Check whether the log file is more recent than the given date. This is
-- solely based on the date that is part of the filename.
isFileNewer :: FilePath -> Date -> Bool
isFileNewer file date = (Just $ base (unpack date)) <= extractFileInfo file
        where base (year':month':day':_) = FileInfo year' month' day' 0
              unpack                  = split "-"

-- | Sort the logfiles by date and log file sequence number
-- The logfile naming scheme is: "atlassian-stash-access-2012-11-29.0.log(.bz2)"
sortLogFiles :: [FilePath] -> [FilePath]
sortLogFiles = sortBy logFilePred
    where sortPred (date1, num1) (date2, num2) = compare date1 date2 `mappend` compare num1 num2
          logFilePred logFileName1 logFileName2 = sortPred (extractSortPairs logFileName1) (extractSortPairs logFileName2)
          extractSortPairs path = maybe ("9999", 0) asPair $ extractFileInfo path
          asPair (FileInfo year' month' day' counter') = (year' ++ "-" ++ month' ++ "-" ++ day', counter')
          asPair _                                     = ("", 0)

-- | Try to extract the FileInfo out of the given file. This function assumes
-- that the given file follows the naming scheme for the access log archive
-- files.
extractFileInfo :: FilePath -> Maybe FileInfo
extractFileInfo path = let elems = drop 3 $ split "-" $ extractFile path
                       in case elems of
                               (year':month':(rest:_)) -> case split "." rest of
                                                             (day':num:_) -> Just $ FileInfo year' month' day' (read num :: Int)
                                                             _           -> Nothing
                               _                     -> Nothing

extractFileDateInfo :: FilePath -> Maybe FileDateInfo
extractFileDateInfo path = fmap (FileDateInfo) $ extractFileInfo path

-- | Read the list of files and return a list of lines. The input files will be
-- filtered using the function (FilePath -> Bool)
--toLines :: (FilePath -> Bool) -> [FilePath] -> IO [L.ByteString]
--toLines p files = liftM L.lines $ readFiles p files

toLines :: [FilePath] -> IO [L.ByteString]
toLines files = liftM L.lines $ readFiles files

-- | Read the list of files and turn them into a lazy ByteString. The input files will be
-- filtered using the function (FilePath -> Bool)
readFiles :: [FilePath] -> IO L.ByteString
readFiles files = trace ("filteredFiles: " ++ show filteredFiles)  fmap L.concat . mapM readCompressedOrUncompressed $ filteredFiles
            where filteredFiles = sortLogFiles files

filterLastDay :: [FilePath] -> [FilePath]
filterLastDay []    = []
filterLastDay files = concat . init . gr $ filterLastFile files
    where filterLastFile    = filter (isJust . extractFileInfo)
          gr                = groupBy (\a b -> fromMaybe False $ liftM2 (==) (extractFileDateInfo a) (extractFileDateInfo b))

dropUntilDate :: Date -> [FilePath] -> [FilePath]
dropUntilDate date files = dropWhile (\f -> not $ isFileNewer f date) files

-- =================================================================================

extractFile :: FilePath -> String
extractFile = last . slice_path

readCompressedOrUncompressed :: FilePath -> IO L.ByteString
readCompressedOrUncompressed path = if ".bz2" `isSuffixOf` path
                                    then liftM BZip.decompress $ L.readFile path
                                    else L.readFile path
data RunConfig = RunConfig {
    cfgProgressive :: Bool
    } deriving (Show)

newRunConfig :: RunConfig
newRunConfig = RunConfig False

readConfig :: String -> IO (Maybe String)
readConfig key = do
        json <- L.readFile "logparser.state"
        return $ (decode json :: Maybe (M.Map String String)) >>= M.lookup key

today :: IO (Integer,Int,Int) -- :: (year,month,day)
today = getCurrentTime >>= return . toGregorian . utctDay

readLogFiles :: RunConfig -> String -> [FilePath] -> IO [L.ByteString]
readLogFiles cfg key path = do
        date <- readConfig key
        now <- today
        let progressive = cfgProgressive cfg
        trace ("date: " ++ show date ++ " key: " ++ key ++ " now: " ++ show now) (if progressive && (isJust date) then
                toLines $ (dropUntilDate $ fromJust date) $ filterLastDay path
            else
                toLines path
                )
