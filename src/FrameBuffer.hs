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

frameBuffer
    :: forall w h a. (KnownNat w, KnownNat h, 1 <= (w * h), NFDataX a)
    => forall dom. (HiddenClockResetEnable dom)
    => a
    -> Signal dom (Maybe (Index (w * h), a))
    -> Signal dom (Maybe (Index w))
    -> Signal dom (Maybe (Index h))
    -> Signal dom (Maybe a)
frameBuffer initial write x y = enable (delay False visible) current
  where
    visible = isJust <$> x .&&. isJust <$> y

    prevY = register Nothing y
    newLine = y ./=. prevY

    rowstride = snatToNum (SNat @w)

    base = register 0 base'
    base' = mux (not <$> newLine) base $
            mux (isNothing <$> prevY) 0 $
            base + rowstride

    address = base' + (maybe 0 fromIntegral <$> x)

    current = blockRam1 ClearOnReset (SNat @(w * h)) initial address write

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (Index (200 * 140), Bit))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (frameEnd, vgaOut (delaySync vgaSync) rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale (SNat @3) . center @600 $ vgaX
    vgaY' = scale (SNat @3) . center @420 $ vgaY

    rgb = maybe <$> grid <*> pure monochrome <*> frameBuffer 0 write vgaX' vgaY'

    grid = mux parity red green
    parity = (maybe 0 lsb <$> vgaX) .==. (maybe 0 lsb <$> vgaY)
    red = pure (255, 0, 0)
    green = pure (0, 255, 0)

delaySync :: (HiddenClockResetEnable dom) => VGASync dom -> VGASync dom
delaySync VGASync{..} = VGASync
    { vgaHSync = register undefined vgaHSync
    , vgaVSync = register undefined vgaVSync
    , vgaDE = register False vgaDE
    }

monochrome :: (Bounded a) => Bit -> a
monochrome 0 = minBound
monochrome 1 = maxBound

makeTopEntity 'topEntity
