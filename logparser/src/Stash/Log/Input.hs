{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Log.Input
( readFiles
, readLogFiles

#ifdef TEST
, sortLogFiles
#endif
) where

import qualified Codec.Compression.BZip     as BZip
import           Control.Monad              (liftM)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (isSuffixOf, sortBy)
import           Data.Monoid                (mappend)
import           Data.String.Utils          (split)
import           System.Path.NameManip



-- | Read the list of files and turn them into a lazy ByteString. The input files will be
-- filtered using the function (FilePath -> Bool)
readFiles :: [FilePath] -> IO L.ByteString
readFiles files = fmap L.concat . mapM readCompressedOrUncompressed $ sortLogFiles files


readLogFiles :: [FilePath] -> IO [L.ByteString]
readLogFiles = toLines

-- =================================================================================

data FileInfo = FileInfo {
     _year    :: String
    ,_month   :: String
    ,_day     :: String
    ,_counter :: Int
} deriving (Show, Ord, Eq)

newtype FileDateInfo = FileDateInfo FileInfo

-- | Ignore the counter for equality checks
instance Eq FileDateInfo where
  (FileDateInfo (FileInfo year1 month1 day1 _)) == (FileDateInfo (FileInfo year2 month2 day2 _))
                            = year1 == year2 && month1 == month2 && day1 == day2

-- | Sort the logfiles by date and log file sequence number
-- The logfile naming scheme is: "atlassian-stash-access-2012-11-29.0.log(.bz2)"
sortLogFiles :: [FilePath] -> [FilePath]
sortLogFiles = sortBy logFilePred
    where sortPred (date1, num1) (date2, num2) = compare date1 date2 `mappend` compare num1 num2
          logFilePred logFileName1 logFileName2 = sortPred (extractSortPairs logFileName1) (extractSortPairs logFileName2)
          extractSortPairs path = maybe ("9999", 0) asPair $ extractFileInfo path
          asPair (FileInfo year' month' day' counter') = (year' ++ "-" ++ month' ++ "-" ++ day', counter')

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
toLines :: [FilePath] -> IO [L.ByteString]
toLines files = liftM L.lines $ readFiles files


-- =================================================================================

extractFile :: FilePath -> String
extractFile = last . slice_path

readCompressedOrUncompressed :: FilePath -> IO L.ByteString
readCompressedOrUncompressed path = if ".bz2" `isSuffixOf` path
                                    then liftM BZip.decompress $ L.readFile path
                                    else L.readFile path
