{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stash.Log.Chart (
    generateGitOperationsChart
  , generateGitDurationChart
  , generateRequestClassificationChart
  , generateMaxConnectionChart
  , generateProtocolStats
  , generateRepositoryStats
) where


import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import qualified Data.ByteString.Char8                  as S
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Function                          (on)
import           Data.List                              (sortBy)
import           Data.Time.LocalTime
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Stash.Log.Analyser
import           Stash.Log.Common
import           Stash.Log.GitOpsAnalyser
import           Stash.Log.Types
import           System.Directory                       (createDirectoryIfMissing)
import           System.FilePath                        ((</>))


-- =================================================================================

generateGitOperationsChart :: String -> FilePath -> [GitOperationStats] -> IO ()
generateGitOperationsChart fileName targetDir xs = do
    -- show all operations in the same graph
    renderChart targetDir fileName (gitOperationsChart xs)
    -- cache hit/miss per operation type
    mapM_ action [Clone, ShallowClone, Push, Fetch, RefAdvertisement]
  where
    action op' = renderChart targetDir (fileName ++ "-" ++ show op') (gitOperationsChart' op' xs)


generateGitDurationChart :: String -> FilePath -> [RequestDurationStat] -> IO ()
generateGitDurationChart fileName targetDir xs =
    mapM_ action [Clone, ShallowClone, Push, Fetch, RefAdvertisement]
  where
    action op' = renderChart targetDir (fileName ++ "-" ++ show op') (gitDurationChart op' xs)

generateRequestClassificationChart :: String -> FilePath -> RequestClassification -> IO ()
generateRequestClassificationChart fileName targetDir RequestClassification{..} = do
    let xs =  [
               ("Git HTTP", gitHttp)
             , ("Git SSH", gitSsh)
             , ("Web UI", webUi)
             , ("File Server", fileServer)
             , ("REST", rest)
            ]
    renderChart targetDir fileName (pieChart "Distribution of operations" xs)


generateMaxConnectionChart :: String -> FilePath -> [DateValuePair] -> IO ()
generateMaxConnectionChart fileName targetDir xs = renderChart targetDir fileName (stackedWithLinesChart "Conncurrent Connections" ([]::[Line Double]) $ toLines xs)
  where
    toLines xs' = [Line ALeft "max concurrent connections" (fmap (\DateValuePair{..} -> (toLocalTime getLogDate, fromIntegral getValue)) xs') blue]

generateProtocolStats :: String -> FilePath -> [ProtocolStats] -> IO ()
generateProtocolStats fileName targetDir xs = renderChart targetDir fileName (linesChart "Git protocol usage" (toLines xs))
  where
    toLines :: [ProtocolStats] -> [Line Double]
    toLines xs' = [
        Line ALeft "http(s)" (fmap (\ProtocolStats{..} -> (toLocalTime getProtocolLogDate, fromIntegral getHttp)) xs') green
      , Line ALeft "ssh" (fmap (\ProtocolStats{..} -> (toLocalTime getProtocolLogDate, fromIntegral getSsh)) xs') red
      ]


generateRepositoryStats :: String -> FilePath -> [RepositoryStat] -> IO ()
generateRepositoryStats fileName targetDir xs = do
    let ys = fmap (\RepositoryStat{..} -> (S.unpack getName ++ " (" ++ show getNumberOfClones ++ ")", fromIntegral getNumberOfClones)) $ take 10 $
             sortBy (flip (compare `on` getNumberOfClones)) (filter statAvailable xs)
    renderChart targetDir fileName (pieChart "Top 10 repositories by number of clones" ys)
  where
    statAvailable StatUnavailable = False
    statAvailable _               = True

-- =================================================================================

data OperationType = Clone | ShallowClone | Fetch | Push | RefAdvertisement deriving (Eq, Show)

data Cache = Hit | Miss deriving (Eq, Show)

type Data a = [(LocalTime,a)]

data Anchor = ALeft | ARight deriving (Eq, Show)

data Line a = Line {
    anchor         :: !Anchor
  , lbl            :: !S.ByteString
  , lineDataPoints :: Data a
  , color          :: Colour Double
} deriving (Eq, Show)

instance (Num a, NFData a) => NFData (Line a) where
    rnf l@Line{..} =
        l {
            lineDataPoints = lineDataPoints `deepseq` lineDataPoints
          , color = color `seq` color
        } `seq` ()

instance NFData LogValue where
    rnf (LogValue d) = d `seq` ()


gitDurationChart :: OperationType -> [RequestDurationStat] -> Renderable ()
gitDurationChart op' xs =
        pointChart "Duration of Git Operations (in seconds)" (toLines op' xs)
  where
     toLines _ []                 = []
     toLines Clone ys             = [Line ALeft "clone/cache hit"              (f cacheHitDurations Clone ys) green
                                   , Line ALeft "clone/cache miss"             (f cacheMissDurations Clone ys) darkred]
     toLines ShallowClone ys      = [Line ALeft "shallow clone/cache hit"      (f cacheHitDurations ShallowClone ys) green
                                   , Line ALeft "shallow clone/cache miss"     (f cacheMissDurations ShallowClone ys) darkred]
     toLines Push ys              = [Line ALeft "push"                         (extractPushes ys) darkgray]
     toLines Fetch ys             = [Line ALeft "fetch/cache hit"              (f cacheHitDurations Fetch ys) green
                                   , Line ALeft "fetch/cache miss"             (f cacheMissDurations Fetch ys) darkred]
     toLines RefAdvertisement ys  = [Line ALeft "ref advertisement/cache hit"  (f cacheHitDurations RefAdvertisement ys) green
                                   , Line ALeft "ref advertisement/cache miss" (f cacheMissDurations RefAdvertisement ys) darkred]
     extractPushes                = fmap (\RequestDurationStat{..} -> (toLocalTime getDurationDate, toLogValue (cacheHitDurations !! 3 + cacheMissDurations !! 3)))
     f fieldF                     = extractField (\x r@RequestDurationStat{..} -> (toLocalTime getDurationDate, toLogValue (fieldF r !! x)))
     toLogValue                   = LogValue . toSeconds


extractField :: (Int -> a -> (LocalTime, b)) -> OperationType -> [a] -> [(LocalTime, b)]
extractField f ot xs
    | ot == Clone            = fmap (f 0) xs
    | ot == ShallowClone     = fmap (f 2) xs
    | ot == Fetch            = fmap (f 1) xs
    | ot == RefAdvertisement = fmap (f 4) xs
    | ot == Push             = fmap (f 3) xs
    | otherwise              = []


gitOperationsChart :: [GitOperationStats] -> Renderable ()
gitOperationsChart xs =
    stackedWithLinesChart "Git Hosting Operations" (refLines xs) (toLines xs)
  where
    toLines []               = []
    toLines ys               = [
                                   Line ALeft "clone" (f Clone ys) blue
                                 , Line ALeft "fetch" (f Fetch ys) darkorange
                                 , Line ALeft "shallow clone" (f ShallowClone ys) green
                                 , Line ALeft "push" (f Push ys) darkgray
                               ]
    refLines []              = []
    refLines ys              = [Line ARight "ref advertisement" (f RefAdvertisement ys) steelblue]
    f                        = extractField (\x GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (cacheHits !! x + cacheMisses !! x)))

gitOperationsChart' :: OperationType -> [GitOperationStats] -> Renderable ()
gitOperationsChart' op' xs =
    stackedWithLinesChart "Git Hosting Operations" (sumLines op' xs) (toLines op' xs)
  where
    toLines _ []                = []
    toLines Clone ys            = [Line ALeft "clone/cache hit" (f cacheHits Clone ys) green
                                 , Line ALeft "clone/cache miss" (f cacheMisses Clone ys) darkred]
    toLines ShallowClone ys     = [Line ALeft "shallow clone/cache hit" (f cacheHits ShallowClone ys) green
                                 , Line ALeft "shallow clone/cache miss" (f cacheMisses ShallowClone ys) darkred]
    toLines Push ys             = [Line ALeft "push" (extractPushes ys) darkgray]
    toLines Fetch ys            = [Line ALeft "fetch/cache hit" (f cacheHits Fetch ys) green
                                 , Line ALeft "fetch/cache miss" (f cacheMisses Fetch ys) darkred]
    toLines RefAdvertisement ys = [Line ALeft "ref advertisement/cache hit" (f cacheHits RefAdvertisement ys) green
                                 , Line ALeft "ref advertisement/cache miss" (f cacheMisses RefAdvertisement ys) darkred]

    sumLines Clone ys            = [Line ALeft "clone" (f cacheHits Clone  ys `add` f cacheMisses Clone ys) blue]
    sumLines ShallowClone ys     = [Line ALeft "shallow clone" (f cacheHits ShallowClone ys `add` f cacheMisses ShallowClone ys) blue]
    sumLines Push _              = []
    sumLines Fetch ys            = [Line ALeft "fetch" (f cacheHits Fetch  ys `add` f cacheMisses Fetch ys) blue]
    sumLines RefAdvertisement ys = [Line ALeft "ref advertisement" (f cacheHits RefAdvertisement ys `add` f cacheMisses RefAdvertisement ys) blue]

    add xs' ys                   = let zs = zip xs' ys
                                   in fmap (\((lk,lv),(_,rv)) -> (lk, lv + rv)) zs

    extractPushes                = fmap (\GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (cacheHits !! 3 + cacheMisses !! 3)))
    f fieldF                     = extractField (\x r@GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (fieldF r !! x)))


pieChart :: String -> [(String, Integer)] -> Renderable ()
pieChart title values = toRenderable layout
  where
    pitem (s,v) = pitem_value .~ fromIntegral v
                  $ pitem_label .~ s
                  $ pitem_offset .~ 0
                  $ def

    layout = pie_title .~ title
           $ pie_plot . pie_data .~ map pitem values
           $ pie_plot . pie_colors  .~ fmap opaque colors
           $ pie_plot . pie_label_style .~ (font_size .~ 16 $ def)
           $ pie_margin .~ 106.0
           $ def


pointChart :: String -> [Line LogValue] -> Renderable ()
pointChart title lines' = do
    let plots = (\Line{..} -> toPlot $ points color lbl lineDataPoints) <$> lines' -- :: [(Plot LocalTime LogValue)]
    toRenderable (layout plots)
  where
    layout p = layout_title .~ title
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ True
           $ layout_plots .~ p
           $ setLayoutForeground (opaque black)
           $ def

    {-points :: Colour LogValue -> String -> Data LogValue -> PlotPoints LocalTime LogValue-}
    points col title' dat = plot_points_style .~ filledCircles 1 (opaque col)
           $ plot_points_values .~ dat
           $ plot_points_title .~ (S.unpack title')
           $ def

linesChart :: (PlotValue a) => String -> [Line a] -> Renderable ()
linesChart title lines' = do
    let plots = (\Line{..} -> toPlot $ single color lbl lineDataPoints) <$> lines' -- :: [(Plot LocalTime LogValue)]
    toRenderable (layout plots)
  where
    single col title' dat = plot_lines_style .~ lineStyle col
           $ plot_lines_values .~ [dat]
           $ plot_lines_title .~ (S.unpack title')
           $ def

    layout p = layout_title .~ title
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_plots .~ p
           $ setLayoutForeground (opaque black)
           $ def

    lineStyle col = line_width .~ 1
              $ line_color .~ opaque col
              $ def

stackedWithLinesChart :: (Num a, PlotValue a) => String -> [Line a] -> [Line a] -> Renderable ()
stackedWithLinesChart title singleLines' lines' = do
    let stacked  = toEither line <$> lines'
        single'  = toEither single <$> singleLines'
        xs       = single' ++ stacked
        hasRight = any isRight xs
    toRenderable (layout xs hasRight)
  where
    line col title' dat = plot_fillbetween_style .~ solidFillStyle (col `withOpacity` 0.4)
           $ plot_fillbetween_values .~ [ (d,(0,v)) | (d,v) <- dat]
           $ plot_fillbetween_title .~ (S.unpack title')
           $ def
    isRight (Left _)  = False
    isRight (Right _) = True
    single col title' dat = plot_lines_style .~ lineStyle col
           $ plot_lines_values .~ [dat]
           $ plot_lines_title .~ (S.unpack title')
           $ def

    layout p showRight = layoutlr_title .~ title
           $ layoutlr_background .~ solidFillStyle (opaque white)
           $ layoutlr_left_axis_visibility . axis_show_ticks .~ False
           $ layoutlr_right_axis_visibility . axis_show_line .~ showRight
           $ layoutlr_right_axis_visibility . axis_show_ticks .~ False
           $ layoutlr_right_axis_visibility . axis_show_labels .~ showRight
           $ layoutlr_plots .~ p
           $ setLayoutLRForeground (opaque black)
           $ def

    lineStyle col = line_width .~ 1
              $ line_color .~ opaque col
              $ def

toEither :: ToPlot a => (Colour Double -> S.ByteString -> Data t -> a x y)
                  -> Line t -> Either (Plot x y) (Plot x y)
toEither f (Line ALeft lbl points col) = Left (toPlot $ f col lbl points)
toEither f (Line ARight lbl points col) = Right (toPlot $ f col lbl points)



renderChart :: FilePath -> String -> Renderable a -> IO ()
renderChart targetDir filename rend = do
    createDirectoryIfMissing True targetDir
    {-_ <- renderableToFile (FileOptions (1200,800) PNG) rend (targetDir </> filename ++ ".png")-}
    _ <- renderableToFile (FileOptions (1200,800) PDF) rend (targetDir </> filename ++ ".pdf")
    return ()


colors :: [Colour Double]
colors = cycle [mightySlate, pacifica, appleChic, cherryPink, grandmasPillow, banquette, strawberrySugar, cremeDeLaCreme, powderPuff, charlotte]



mightySlate, pacifica, appleChic, cherryPink, grandmasPillow, banquette, strawberrySugar,
  cremeDeLaCreme, powderPuff, charlotte :: Colour Double
mightySlate = sRGB' 85.0 98.0 112.0

pacifica = sRGB' 78.0 205.0 196.0

appleChic = sRGB' 199.0 244.0 100.0

cherryPink = sRGB' 255.0 107.0 107.0

grandmasPillow = sRGB' 196.0 77.0 88.0

banquette = sRGB' 119.0 79.0 56.0

strawberrySugar = sRGB' 224.0 142.0 121.0

cremeDeLaCreme = sRGB' 241.0 212.0 175.0

powderPuff = sRGB' 236.0 229.0 206.0

charlotte = sRGB' 197.0 224.0 220.0

sRGB' :: Double -> Double -> Double -> Colour Double
sRGB' a b c = sRGB (toR a) (toR b) (toR c)

toR :: Double -> Double
toR i = i/255.0

toDouble :: Int -> Double
toDouble = (1.0 *) . fromIntegral

-- |
--
-- >>> toSeconds (Millis 1200)
-- 1.2
toSeconds :: Millis -> Double
toSeconds = (/ 1000.0) . fromIntegral . millis
