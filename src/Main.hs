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
    _ -> error ("Invoke with <cmd> <path-to-log-file>" ++ "\n\nAvailable commands: " ++ (show $ map fst actions))

dispatch :: Command -> FilePath -> IO ()
dispatch cmd = action
    where
        action = fromMaybe err (lookup cmd actions)
        err _  = putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

actions :: [(Command, FilePath -> IO ())]
actions = [("count", countLogFileLines)
          ,("countRequests",countRequests)
          ,("show", showParsedLines)
          ,("maxConn", showMaxConcurrent)
          ,("plotConnMinute", generatePlotDataConcurrentConn plotDataConcurrentConnMinute)
          ,("plotConnHour", generatePlotDataConcurrentConn plotDataConcurrentConnHour)
          ,("protocol", mapToTopList protocolCount)]

countRequests :: FilePath -> IO()
countRequests path = parseAndPrint path countRequestLines

showParsedLines :: FilePath -> IO()
showParsedLines path = parseAndPrint path showLines

countLogFileLines :: FilePath -> IO ()
countLogFileLines path = parseAndPrint path countLines

showMaxConcurrent :: FilePath -> IO ()
showMaxConcurrent path = parseAndPrint path maxConcurrent

generatePlotDataConcurrentConn :: ([L.ByteString] -> [DateValuePair]) -> FilePath -> IO ()
generatePlotDataConcurrentConn f path = do
        content <- L.readFile path
        let input = L.lines content
        let plotData = f input
        mapM_ (\pd -> printf "%s|%d\n" (formatLogDate $ getLogDate pd) (getValue pd)) plotData

parseAndPrint :: (Show a) => FilePath -> ([L.ByteString] -> a) -> IO ()
parseAndPrint path f = print . f . L.lines =<< L.readFile path


mapToTopList :: ([L.ByteString] -> [(S.ByteString, Integer)]) -> FilePath -> IO ()
mapToTopList f p = do
    file <- liftM L.lines $ L.readFile p
    let mostPopular (_,a) (_,b) = compare b a
        m = f file
    mapM_ putStrLn . zipWith pretty [1..] . take 10 . sortBy mostPopular $ m

formatLogDate :: LogDate -> String
formatLogDate date = printf "%04d-%02d-%02d %02d:%02d" (getYear date) (getMonth date)
                            (getDay date) (getHour date) (getMinute date)

pretty :: Show a => Integer -> (a, Integer) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n
