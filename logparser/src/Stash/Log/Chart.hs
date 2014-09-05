{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stash.Log.Chart (
  generateChart
) where

import           Control.Applicative
import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Time.LocalTime
import           Data.Default.Class
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Stash.Log.Common
import           Stash.Log.GitOpsAnalyser
import           System.Directory                       (getDirectoryContents, createDirectoryIfMissing)
import           System.FilePath                        ((</>), takeBaseName)

type Data = [(LocalTime,Double)]

data Anchor = ALeft | ARight deriving (Eq, Show)

data Line = Line {
    anchor         :: Anchor
  , lbl            :: String
  , lineDataPoints :: Data
} deriving (Eq, Show)

{-
data GitOperationStats = GitOperationStats {
    getOpStatDate :: !String
  , cacheMisses   :: ![Int] -- clone, fetch, shallow clone, push, ref advertisement
  , cacheHits     :: ![Int]
}
-}

chart :: [GitOperationStats] -> Renderable ()
chart xs =
        stackedWithLinesChart "Git Hosting Operations" (refLines xs) (toLines xs)
  where
     toLines []               = []
     toLines ys               = [
                                    Line ALeft "clone" (extractClones ys)
                                  , Line ALeft "fetch" (extractFetches ys)
                                  , Line ALeft "shallow clone" (extractShallowClones ys)
                                  , Line ALeft "push" (extractPushes ys)
                                ]
     refLines []              = []
     refLines ys              = [Line ARight "ref advertisement" (extractRefAdvertisements ys)]
     extractClones            = extractField 0
     extractFetches           = extractField 1
     extractShallowClones     = extractField 2
     extractPushes            = extractField 3
     extractRefAdvertisements = extractField 4
     extractField x           = fmap (\GitOperationStats{..} -> (toLocalTime getOpStatDate, toDouble (cacheHits !! x + cacheMisses !! x)))


colors :: [Colour Double]
colors = cycle [blue, darkorange, green, darkred, darkgray, steelblue, yellow, black]


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
-- lineChart :: String -> [Line] -> Renderable ()
-- lineChart title lines' = do
--     let plots = (\(Line{..}, col) -> toPlot $ line col lbl lineDataPoints) <$> zip lines' colors
--     toRenderable (layout plots)
--   where
--     layout p = layout_title .~ title
--            $ layout_background .~ solidFillStyle (opaque white)
--            $ layout_left_axis_visibility . axis_show_ticks .~ True
--            $ layout_plots .~ p
--            $ setLayoutForeground (opaque black)
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
-- stackedChart :: String -> [Line] -> Renderable ()
-- stackedChart title = stackedWithLinesChart title []

stackedWithLinesChart :: String -> [Line] -> [Line] -> Renderable ()
stackedWithLinesChart title singleLines' lines' = do
    let stacked = uncurry (toEither line) <$> zip lines' colors
        single' = uncurry (toEither single) <$> zip singleLines' colors :: [Either (Plot LocalTime Double) (Plot LocalTime Double)]
    toRenderable (layout $ single' ++ stacked)
  where
    line col title' dat = plot_fillbetween_style .~ solidFillStyle (col `withOpacity` 0.4)
           $ plot_fillbetween_values .~ [ (d,(0,v)) | (d,v) <- dat]
           $ plot_fillbetween_title .~ title'
           $ def

    single col title' dat = plot_lines_style .~ lineStyle col
           $ plot_lines_values .~ [dat]
           $ plot_lines_title .~ title'
           $ def

    layout p = layoutlr_title .~ title
           $ layoutlr_background .~ solidFillStyle (opaque white)
           $ layoutlr_left_axis_visibility . axis_show_ticks .~ False
           $ layoutlr_plots .~ p
           $ setLayoutLRForeground (opaque black)
           $ def

    lineStyle col = line_width .~ 1
              $ line_color .~ opaque col
              $ def

    toEither f (Line ALeft lbl points) col = Left (toPlot $ f col lbl points)
    toEither f (Line ARight lbl points) col = Right (toPlot $ f col lbl points)

generateChart fileName targetDir xs = renderChart targetDir fileName (chart xs)


renderChart :: FilePath -> String -> Renderable a -> IO ()
renderChart targetDir filename rend = do
    createDirectoryIfMissing True targetDir
    _ <- renderableToFile (FileOptions (1200,800) PDF) rend (targetDir </> filename ++ ".pdf")
    return ()


toDouble :: Int -> Double
toDouble x = fromIntegral x * 1.0
