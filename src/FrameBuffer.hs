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
    -> DSignal dom 1 (Maybe a)
frameBuffer initial write x y = enable (delayedI False visible) current
  where
    visible = fromSignal $ isJust <$> x .&&. isJust <$> y

    prevY = register Nothing y
    newLine = y ./=. prevY

    rowstride = snatToNum (SNat @w)

    base = register 0 base'
    base' = mux (not <$> newLine) base $
            mux (isNothing <$> prevY) 0 $
            base + rowstride

    address = base' + (maybe 0 fromIntegral <$> x)

    current = delayedBlockRam1 ClearOnReset (SNat @(w * h)) initial (fromSignal address) (fromSignal write)

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (Index (200 * 140), Bit))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale (SNat @3) . center @600 $ vgaX
    vgaY' = scale (SNat @3) . center @420 $ vgaY

    rgb = maybe <$> grid <*> pure monochrome <*> frameBuffer 0 write vgaX' vgaY'

    grid = toDelayedU $ mux parity red green
    parity = (maybe 0 lsb <$> vgaX) .==. (maybe 0 lsb <$> vgaY)
    red = pure (255, 0, 0)
    green = pure (0, 255, 0)

delaySync :: (HiddenClockResetEnable dom) => VGASync dom -> VGASync dom
delaySync VGASync{..} = VGASync
    { vgaHSync = register undefined vgaHSync
    , vgaVSync = register undefined vgaVSync
    , vgaDE = register False vgaDE
    }

delayVGA
    :: (KnownNat d, KnownNat r, KnownNat g, KnownNat b)
    => (HiddenClockResetEnable dom)
    => VGASync dom
    -> DSignal dom d (Unsigned r, Unsigned g, Unsigned b)
    -> VGAOut dom r g b
delayVGA VGASync{..} rgb = vgaOut vgaSync' (toSignal rgb)
  where
    vgaSync' = VGASync
        { vgaHSync = matchDelay rgb vgaHSync
        , vgaVSync = matchDelay rgb vgaVSync
        , vgaDE = matchDelay rgb vgaDE
        }

matchDelay
    :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
    => DSignal dom d any
    -> Signal dom a
    -> Signal dom a
matchDelay d = toSignal . (d *>) . toDelayedU

delayedU
  :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
  => DSignal dom n a
  -> DSignal dom (n + d) a
delayedU = delayedI undefined

toDelayedU
  :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
  => Signal dom a
  -> DSignal dom d a
toDelayedU = delayedU . fromSignal

delayedBlockRam1
    :: (1 <= m, Enum addr, NFDataX a, HiddenClockResetEnable dom)
    => ResetStrategy r
    -> SNat m
    -> a
    -> DSignal dom d addr
    -> DSignal dom d (Maybe (addr, a))
    -> DSignal dom (d + 1) a
delayedBlockRam1 resetStrat size content addr wr = unsafeFromSignal $
    blockRam1 resetStrat size content (toSignal addr) (toSignal wr)

monochrome :: (Bounded a) => Bit -> a
monochrome 0 = minBound
monochrome 1 = maxBound

makeTopEntity 'topEntity
