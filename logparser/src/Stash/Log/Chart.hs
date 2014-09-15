{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stash.Log.Chart (
    generateGitOperationsChart
  , generateGitDurationChart
) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Time.LocalTime
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
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
    action op' = renderChart targetDir (fileName ++ "-" ++ (show op')) (gitOperationsChart' op' xs)


generateGitDurationChart :: String -> FilePath -> [RequestDurationStat] -> IO ()
generateGitDurationChart fileName targetDir xs = do
    mapM_ action [Clone, ShallowClone, Push, Fetch, RefAdvertisement]
  where
    action op' = renderChart targetDir (fileName ++ "-" ++ (show op')) (gitDurationChart op' xs)

-- =================================================================================

data OperationType = Clone | ShallowClone | Fetch | Push | RefAdvertisement deriving (Eq, Show)

type Data a = [(LocalTime,a)]

data Anchor = ALeft | ARight deriving (Eq, Show)

data Line a = Line {
    anchor         :: !Anchor
  , lbl            :: !String
  , lineDataPoints :: Data a
  , color          :: Colour Double
} deriving (Eq, Show)

instance (Num a, NFData a) => NFData (Line a) where
    rnf l@Line{..} =
        l {
            anchor = anchor `seq` anchor
          , lbl = lbl `seq` lbl
          , lineDataPoints = lineDataPoints `deepseq` lineDataPoints
          , color = color `seq` color
        } `seq` ()

instance NFData LogValue where
    rnf (LogValue d) = d `seq` ()


gitDurationChart :: OperationType -> [RequestDurationStat] -> Renderable ()
gitDurationChart op' xs =
        pointChart "Duration of Git Operations (in seconds)" (toLines op' xs)
  where
     toLines _ []                 = []
     toLines Clone ys             = [Line ALeft "clone/cache hit" (extractClonesHit ys) green, Line ALeft "clone/cache miss" (extractClonesMiss ys) darkred ]
     toLines ShallowClone ys      = [Line ALeft "shallow clone/cache hit" (extractShallowClonesHit ys) green
                                   , Line ALeft "shallow clone/cache miss" (extractShallowClonesMiss ys) darkred ]
     toLines Push ys              = [Line ALeft "push" (extractPushes ys) darkgray]
     toLines Fetch ys             = [Line ALeft "fetch/cache hit" (extractFetchesHit ys) green, Line ALeft "fetch/cache miss" (extractFetchesMiss ys) darkred ]
     toLines RefAdvertisement ys  = [Line ALeft "ref advertisement/cache hit" (extractRefAdvertisementsHit ys) green, Line ALeft "ref advertisement/cache miss" (extractRefAdvertisementsMiss ys) darkred ]
     extractClonesHit             = extractField 0 cacheHitDurations
     extractClonesMiss            = extractField 0 cacheMissDurations
     extractFetchesHit            = extractField 1 cacheHitDurations
     extractFetchesMiss           = extractField 1 cacheMissDurations
     extractShallowClonesHit      = extractField 2 cacheHitDurations
     extractShallowClonesMiss     = extractField 2 cacheMissDurations
     extractPushes                = fmap (\RequestDurationStat{..} -> (toLocalTime getDurationDate, toLogValue (cacheHitDurations !! 3 + cacheMissDurations !! 3)))
     extractRefAdvertisementsHit  = extractField 4 cacheHitDurations
     extractRefAdvertisementsMiss = extractField 4 cacheMissDurations
     extractField x f             = fmap (\r@RequestDurationStat{..} -> (toLocalTime getDurationDate, toLogValue ((f r) !! x)))
     toLogValue                   = LogValue . toSeconds


gitOperationsChart :: [GitOperationStats] -> Renderable ()
gitOperationsChart xs =
    stackedWithLinesChart "Git Hosting Operations per hour" (refLines xs) (toLines xs)
  where
    toLines []               = []
    toLines ys               = [
                                   Line ALeft "clone" (extractClones ys) blue
                                 , Line ALeft "fetch" (extractFetches ys) darkorange
                                 , Line ALeft "shallow clone" (extractShallowClones ys) green
                                 , Line ALeft "push" (extractPushes ys) darkgray
                               ]
    refLines []              = []
    refLines ys              = [Line ARight "ref advertisement" (extractRefAdvertisements ys) steelblue]
    extractClones            = extractField 0
    extractFetches           = extractField 1
    extractShallowClones     = extractField 2
    extractPushes            = extractField 3
    extractRefAdvertisements = extractField 4
    extractField x           = fmap (\GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (cacheHits !! x + cacheMisses !! x)))

gitOperationsChart' :: OperationType -> [GitOperationStats] -> Renderable ()
gitOperationsChart' op' xs =
    stackedWithLinesChart "Git Hosting Operations per hour" (sumLines op' xs) (toLines op' xs)
  where
    toLines _ []                = []
    toLines Clone ys            = [Line ALeft "clone/cache hit" (extractClonesHit ys) green, Line ALeft "clone/cache miss" (extractClonesMiss ys) darkred ]
    toLines ShallowClone ys     = [Line ALeft "shallow clone/cache hit" (extractShallowClonesHit ys) green
                                 , Line ALeft "shallow clone/cache miss" (extractShallowClonesMiss ys) darkred ]
    toLines Push ys             = [Line ALeft "push" (extractPushes ys) darkgray]
    toLines Fetch ys            = [Line ALeft "fetch/cache hit" (extractFetchesHit ys) green, Line ALeft "fetch/cache miss" (extractFetchesMiss ys) darkred ]
    toLines RefAdvertisement ys = [Line ALeft "ref advertisement/cache hit" (extractRefAdvertisementsHit ys) green, Line ALeft "ref advertisement/cache miss" (extractRefAdvertisementsMiss ys) darkred ]

    sumLines Clone ys            = [Line ALeft "clone/cache hit" ((extractClonesHit ys) `add` (extractClonesMiss ys)) blue]
    sumLines ShallowClone ys     = [Line ALeft "shallow clone/cache hit" (extractShallowClonesHit ys `add` extractShallowClonesMiss ys) blue]
    sumLines Push ys             = []
    sumLines Fetch ys            = [Line ALeft "fetch/cache hit" (extractFetchesHit ys `add` extractFetchesMiss ys) blue]
    sumLines RefAdvertisement ys = [Line ALeft "ref advertisement" (extractRefAdvertisementsHit ys `add` extractRefAdvertisementsMiss ys) blue]

    add xs ys                    = let zs = zip xs ys
                                   in fmap (\((lk,lv),(_,rv)) -> (lk, lv + rv)) zs

    extractClonesHit             = extractField 0 cacheHits
    extractClonesMiss            = extractField 0 cacheMisses
    extractFetchesHit            = extractField 1 cacheHits
    extractFetchesMiss           = extractField 1 cacheMisses
    extractShallowClonesHit      = extractField 2 cacheHits
    extractShallowClonesMiss     = extractField 2 cacheMisses
    extractPushes                = fmap (\GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (cacheHits !! 3 + cacheMisses !! 3)))
    extractRefAdvertisementsHit  = extractField 4 cacheHits
    extractRefAdvertisementsMiss = extractField 4 cacheMisses
    extractField x f             = fmap (\r@GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble ((f r) !! x)))

-- colors :: [Colour Double]
-- colors = cycle [blue, darkorange, green, darkred, darkgray, steelblue, yellow, black]


-- chartLeftRight :: String -> [Line] -> Renderable ()
-- chartLeftRight title lines' = do
--     let plots = uncurry toEither <$> zip lines' colors :: [Either (Plot LocalTime Double) (Plot LocalTime Double)]
--     toRenderable (layout plots)
--   where
--     layout p = layoutlr_title .~ title
--            $ layoutlr_background .~ solidFillStyle (opaque white)
--            $ layoutlr_left_axis_visibility . axis_show_ticks .~ False
--            $ layoutlr_plots .~ p
--            $ setLayoutLRForeground (opaque black)
--            $ def
--
--     line col title' dat = plot_lines_style .~ lineStyle col
--            $ plot_lines_values .~ [dat]
--            $ plot_lines_title .~ title'
--            $ def
--
--     lineStyle col = line_width .~ 1
--               $ line_color .~ opaque col
--               $ def
--
--     toEither (Line ALeft lbl points) col = Left (toPlot $ line col lbl points)
--     toEither (Line ARight lbl points) col = Right (toPlot $ line col lbl points)
--
--
--
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
           $ plot_points_title .~ title'
           $ def


-- stackedChart :: String -> [Line] -> Renderable ()
-- stackedChart title = stackedWithLinesChart title []

stackedWithLinesChart :: String -> [Line Double] -> [Line Double] -> Renderable ()
stackedWithLinesChart title singleLines' lines' = do
    let stacked  = (toEither line) <$> lines'
        single'  = (toEither single) <$> singleLines' :: [Either (Plot LocalTime Double) (Plot LocalTime Double)]
        xs       = single' ++ stacked
        hasRight = any isRight xs
    toRenderable (layout xs hasRight)
  where
    line col title' dat = plot_fillbetween_style .~ solidFillStyle (col `withOpacity` 0.4)
           $ plot_fillbetween_values .~ [ (d,(0,v)) | (d,v) <- dat]
           $ plot_fillbetween_title .~ title'
           $ def
    isRight (Left _)  = False
    isRight (Right _) = True
    single col title' dat = plot_lines_style .~ lineStyle col
           $ plot_lines_values .~ [dat]
           $ plot_lines_title .~ title'
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

    toEither f (Line ALeft lbl points col) = Left (toPlot $ f col lbl points)
    toEither f (Line ARight lbl points col) = Right (toPlot $ f col lbl points)



renderChart :: FilePath -> String -> Renderable a -> IO ()
renderChart targetDir filename rend = do
    createDirectoryIfMissing True targetDir
    {-_ <- renderableToFile (FileOptions (1200,800) PNG) rend (targetDir </> filename ++ ".png")-}
    _ <- renderableToFile (FileOptions (1200,800) PDF) rend (targetDir </> filename ++ ".pdf")
    return ()


toDouble :: Int -> Double
toDouble = (1.0 *) . fromIntegral

-- |
--
-- >>> toMinutes (Millis 1200)
-- 2.0e-2
-- toMinutes :: Millis -> Double
-- toMinutes = (/ (60.0 * 1000)) . fromIntegral . millis

-- |
--
-- >>> toSeconds (Millis 1200)
-- 12
toSeconds :: Millis -> Double
toSeconds = (/ 1000.0) . fromIntegral . millis
