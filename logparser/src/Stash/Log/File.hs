{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Stash.Log.File
( sortLogFiles
, toLines
, readFiles
) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Codec.Compression.BZip as BZip
import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.List (isSuffixOf)
import Data.String.Utils (split)
import System.Path.NameManip
import Control.Monad (liftM)

-- | Sort the logfiles by date and log file sequence number
-- The logfile naming scheme is: "atlassian-stash-access-2012-11-29.0.log(.bz2)"
sortLogFiles :: [FilePath] -> [FilePath]
sortLogFiles = sortBy logFilePred
    where extractFile = last . slice_path
          sortPred (date1, num1) (date2, num2) = compare date1 date2 `mappend` compare num1 num2
          logFilePred logFileName1 logFileName2 = sortPred (extractSortPairs logFileName1) (extractSortPairs logFileName2)
          extractSortPairs path = let elems = drop 3 $ split "-" $ extractFile path
                                  in case elems of
                                     (year:month:(rest:_)) -> case split "." rest of
                                                             (day:num:_) -> (year ++ "-" ++ month ++ "-" ++ day, read num :: Int)
                                                             _           -> ("", 0)
                                     _                 -> ("9999", 0)


toLines :: (FilePath -> Bool) -> [FilePath] -> IO [L.ByteString]
toLines p files = liftM L.lines $ readFiles p files

readFiles :: (FilePath -> Bool) -> [FilePath] -> IO L.ByteString
readFiles f files = fmap L.concat . mapM readCompressedOrUncompressed . filter f $ sortLogFiles files

readCompressedOrUncompressed :: FilePath -> IO L.ByteString
readCompressedOrUncompressed path = if ".bz2" `isSuffixOf` path
                                    then liftM BZip.decompress $ L.readFile path
                                    else L.readFile path
