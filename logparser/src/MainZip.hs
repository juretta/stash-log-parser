{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import System.Environment (getArgs, withArgs)
import Stash.Support.ZipFile
import System.Console.CmdArgs
import Prelude hiding (takeWhile)

-- =================================================================================

appName :: String
appName = "stash-support"

appVersion :: String
appVersion = "1.0"

appShortDesc :: String
appShortDesc = "Analyse the Atlassian Stash Support Zip"

data SupportZip = RepositoryInformation            {file :: FilePath}
             deriving (Data,Typeable,Show,Eq)


repositoryInformation :: SupportZip
repositoryInformation = RepositoryInformation {file = def &= args}
                &= name "repositoryInformation" &= help "Extract repository information\
                \ from the given support zip file"

mode :: Mode (CmdArgs SupportZip)
mode = cmdArgsMode $ modes [repositoryInformation]
        &= help appShortDesc
        &= program appName &= summary (appName ++ " " ++ appVersion)
        &= verbosity


run :: SupportZip -> IO ()
run (RepositoryInformation file') = extractRepositoryInformation file'

main :: IO ()
main = do
    options <- getArgs
    -- We need arguments so if there are no arguments given, invoke the help command
    config <- (if null options then withArgs ["--help"] else id) $ cmdArgsRun mode
    run config

