module Main where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Stash.Log.Parser
import Stash.Log.Analyser
import System.Environment (getArgs)
import Prelude hiding (takeWhile)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.List (sortBy)
import Control.Monad (liftM)

-- =================================================================================

type Command = String

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd, path] -> dispatch cmd path
    _ -> error ("Invoke with <cmd> <path-to-log-file>" ++ "\n\nAvailable commands: " ++ show (map fst actions))

dispatch :: Command -> FilePath -> IO ()
dispatch cmd = action
    where
        action = fromMaybe err (lookup cmd actions)
        err _  = putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

actions :: [(Command, FilePath -> IO ())]
actions = [("count", parseAndPrint countLines)
          ,("countRequests", parseAndPrint countRequestLines)
          ,("show", parseAndPrint showLines)
          ,("maxConn", parseAndPrint maxConcurrent)
          ,("plotConnMinute", generatePlotDataConcurrentConn plotDataConcurrentConnMinute)
          ,("plotConnHour", generatePlotDataConcurrentConn plotDataConcurrentConnHour)
          ,("plotGitOperations", generatePlotDataGitOps plotGitOperations)
          ,("protocol", mapToTopList protocolCount)]

generatePlotDataGitOps :: (Input -> [(String, Int, Int, Int, Int)]) -> FilePath -> IO ()
generatePlotDataGitOps f path = do
        plotData <- liftM f $ toLines path
        mapM_ (\(date, a, b, c, d)  -> printf "%s|%d|%d|%d|%d\n" date a b c d) plotData

generatePlotDataConcurrentConn :: (Input -> [DateValuePair]) -> FilePath -> IO ()
generatePlotDataConcurrentConn f path = do
        plotData <- liftM f $ toLines path
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

parseAndPrint :: (Show a) => (Input -> a) -> FilePath -> IO ()
parseAndPrint f path = print . f . L.lines =<< L.readFile path

mapToTopList :: (Input -> [(S.ByteString, Integer)]) -> FilePath -> IO ()
mapToTopList f path = do
    result <- toLines path
    let mostPopular (_,a) (_,b) = compare b a
        m = f result
    mapM_ putStrLn . zipWith pretty [1..] . take 10 . sortBy mostPopular $ m

toLines :: FilePath -> IO [L.ByteString]
toLines path = liftM L.lines $ L.readFile path

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)

pretty :: Show a => Integer -> (a, Integer) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n
