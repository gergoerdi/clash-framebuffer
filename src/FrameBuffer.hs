{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module FrameBuffer where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET" ::: Reset Dom25
    -> "VGA" ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen vga
  where
    (frameEnd, vga) = video write

    ptr = regEn 0 frameEnd $ ptr + 1
    write = packWrite <$> ptr <*> pure (Just 1)

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (Index (640 * 480), Bit))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (frameEnd, vgaOut vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    rgb = monochrome <$> current

    frameEnd = isFalling False (isJust <$> vgaY)
    lineEnd = isFalling False (isJust <$> vgaX)

    base = regEn 0 lineEnd $ mux (isNothing <$> vgaY) 0 (base + 640)
    address = base + (maybe 0 fromIntegral <$> vgaX)

    current = blockRam1 ClearOnReset (SNat @(640 * 480)) 0 address write

monochrome :: (Bounded a) => Bit -> a
monochrome 0 = minBound
monochrome 1 = maxBound

makeTopEntity 'topEntity
