{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.File
( sortLogFiles
, toLines
, readFiles
, FileInfo(..)
, extractFileInfo
, isFileNewer
) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Codec.Compression.BZip as BZip
import Data.Monoid (mappend)
import Data.List (isSuffixOf, sortBy)
import Data.String.Utils (split)
import System.Path.NameManip
import Control.Monad (liftM)
import Debug.Trace

data FileInfo = FileInfo {
     year       :: String
    ,month      :: String
    ,day        :: String
    ,counter    :: Int
} deriving (Show, Eq, Ord)

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

-- | Read the list of files and return a list of lines. The input files will be
-- filtered using the function (FilePath -> Bool)
toLines :: (FilePath -> Bool) -> [FilePath] -> IO [L.ByteString]
toLines p files = liftM L.lines $ readFiles p files

-- | Read the list of files and turn them into a lazy ByteString. The input files will be
-- filtered using the function (FilePath -> Bool)
readFiles :: (FilePath -> Bool) -> [FilePath] -> IO L.ByteString
readFiles f files = trace ("filteredFiles: " ++ show filteredFiles)  fmap L.concat . mapM readCompressedOrUncompressed $ filteredFiles
            where filteredFiles = filter f $ sortLogFiles files

-- =================================================================================

extractFile :: FilePath -> String
extractFile = last . slice_path

readCompressedOrUncompressed :: FilePath -> IO L.ByteString
readCompressedOrUncompressed path = if ".bz2" `isSuffixOf` path
                                    then liftM BZip.decompress $ L.readFile path
                                    else L.readFile path
