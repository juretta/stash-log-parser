{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Stash.Support.ZipFile (
    extractRepositoryInformation
) where

import qualified     Data.Conduit.Binary         as CB
import "zip-conduit" Codec.Archive.Zip
import               Control.Monad (join, when)
import               Data.Maybe (mapMaybe)
import               Text.XML.HaXml  hiding (when)
import               Text.XML.HaXml.Posn
import               Text.XML.HaXml.Util
import               Text.XML.HaXml.Xtract.Parse
import               Text.Printf
import               Data.Attoparsec.Text   hiding (take)
import               Control.Applicative
import qualified     Data.Text as T
import               Data.List (sortBy)
import               System.Directory (doesFileExist)

findNodeList :: String -> Content Posn -> [Content Posn]
findNodeList = xtract id

toContent :: Element Posn -> Content Posn
toContent el = CElem el noPos

parseXmlString :: String -> Element Posn
parseXmlString ct =
    let Document _ _ root _ = xmlParse "" ct
    in root

data Size = Byte {
    bytes :: Int
} deriving (Eq, Show)

instance Ord Size where
    compare (Byte a) (Byte b) = compare a b

parseSize :: T.Text -> Maybe Size
parseSize = maybeResult . parse par
    where par :: Parser Size
          par = do
            b <- decimal
            d <- asciiCI "KB" <|> asciiCI "MB"
            return $ Byte (m d b)
            where m "KB" = (* 1024)       -- assuming kibi
                  m "MB" = (* 1024^2)     -- assuming mibi
                  m "GB" = (* 1024^3)     -- assuming gibi
                  m _    = id


extractRepositoryInformation :: FilePath -> IO ()
extractRepositoryInformation archivePath = do
    fileExists <- doesFileExist archivePath
    when fileExists $ do
        fileName <- extract archivePath
        parseXml fileName
        return ()

extract :: FilePath -> IO FilePath
extract archivePath = do
    let fileName = "application.xml"
    withArchive archivePath $ sourceEntry "application-properties/application.xml" $ CB.sinkFile fileName
    return fileName

parseXml :: FilePath -> IO ()
parseXml fileName = do
    content <- readFile fileName
    let xml  = toContent $ parseXmlString content
        -- stashInformation         = findNodeList "//stash-information/(build-version + build-number)" xml
        -- machineInformation       = findNodeList "//java-runtime-environment/(max-heap)" xml
        repositories             = findNodeList "//repository" xml
        mapElement (Elem (N qname') _ xs) = Just (qname', join $ fmap verbatim xs)
        mapElement _             = Nothing
        extractNameAndSize       = mapMaybe (mapElement . contentElem) . cat [childrenBy (tag "name"), childrenBy (tag "size")]
        toTuple (a:b:_)          = Just (snd a, maybe 0 bytes $ parseSize $ T.pack $ snd b, snd b)
        toTuple _                = Nothing
        printNameAndSize (name,bytes',size) = printf "%s | %d | %s \n" name bytes' size

    mapM_ printNameAndSize $ sortBy (\(_,s,_) (_,s2,_) -> compare s2 s) $ mapMaybe (toTuple . extractNameAndSize) repositories



