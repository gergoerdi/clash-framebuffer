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
topEntity = withEnableGen board
  where
    board = vga
      where
        (frameEnd, vga) = video write

        ptr = regEn (0, 0) frameEnd $ nextPtr <$> ptr
        write = packWrite <$> (fbAddress <$> ptr) <*> pure (Just 1)

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (Unsigned FBWidth, Bit))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (frameEnd, vgaOut vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    rgb = monochrome <$> current

    frameEnd = isFalling False (isJust <$> vgaY)
    vgaXY = bundle (fromMaybe 0 <$> vgaX, fromMaybe 0 <$> vgaY)

    addr = fbAddress <$> vgaXY
    current = blockRam1 ClearOnReset (SNat @FBSize) 0 addr write

type FBWidth = CLog 2 480 + CLog 2 640
type FBSize = 2 ^ FBWidth

fbAddress :: (Index 640, Index 480) -> Unsigned FBWidth
fbAddress (x, y) = bitCoerce (y, x)

nextPtr :: (Index 640, Index 480) -> (Index 640, Index 480)
nextPtr (x, y) = case succIdx x of
    Just x' -> (x', y)
    Nothing -> (0, nextIdx y)

monochrome :: (Bounded a) => Bit -> a
monochrome 0 = minBound
monochrome 1 = maxBound

makeTopEntity 'topEntity
