{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module FrameBuffer.Text where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import RetroClash.Delayed
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

    frameEnd' = riseEvery (SNat @10_000)
    ptr = regEn 0 frameEnd' $ satAdd SatWrap 1 <$> ptr
    char = regEn 0 frameEnd' $ char + 1
    write = packWrite <$> ptr <*> (Just <$> char)

    -- ptr = regEn 0 frameEnd' $ ptr + 1
    -- char = regEn 0 frameEnd' $ char + 1

frameBuffer
    :: forall w h a. (KnownNat w, KnownNat h, 1 <= (w * h), NFDataX a)
    => forall dom. (HiddenClockResetEnable dom)
    => a
    -> Signal dom (Maybe (Index (w * h), a))
    -> Signal dom (Maybe (Index w))
    -> Signal dom (Maybe (Index h))
    -> DSignal dom 1 (Maybe a)
frameBuffer initial write x y = enable (delayI False visible) current
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
    => Signal Dom25 (Maybe (Index (40 * 25), Unsigned 8))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (matchDelay rgb False frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale (SNat @2) vgaX
    vgaY' = scale (SNat @2) . center @400 $ vgaY

    rgb = maybe <$> delayI undefined grid <*> pure monochrome <*> fontPixel

    textX = fmap (fromIntegral @_ @(Index 40) . (`shiftR` 3)) <$> vgaX'
    textY = fmap (fromIntegral @_ @(Index 25) . (`shiftR` 3)) <$> vgaY'

    fontX0 = fmap (fromIntegral @_ @(Unsigned 3)) <$> vgaX'

    fontX = delayI undefined $ fromSignal fontX0
    fontY = delayI undefined $ fromSignal $ fmap (fromIntegral @_ @(Unsigned 3)) <$> vgaY'

    char = frameBuffer (0 :: Unsigned 8) write textX textY
    fontAddr = do
        char <- char
        fontY <- fontY
        pure $ bitCoerce @(Unsigned 8, Unsigned 3) <$> ((,) <$> char <*> fontY)
    fontLoad = delayedRom (romFilePow2 @11 @8 "seabios8x8.rom") (fromMaybe 0 <$> fontAddr)

    fontRow = register (0 :: BitVector 8) $
              mux (textX ./=. register Nothing textX) (toSignal fontLoad) $
              mux (fontX0 ./=. register Nothing fontX0) ((`shiftL` 1) <$> fontRow) $
              fontRow

    fontPixel = enable (isJust <$> fontX .&&. isJust <$> fontY) $ fromSignal $ msb <$> fontRow

    grid = fromSignal $ mux parity red green
    parity = (maybe 0 lsb <$> vgaX) .==. (maybe 0 lsb <$> vgaY)
    red = pure (255, 0, 0)
    green = pure (0, 255, 0)

-- monochrome :: (Bounded a) => Bit -> a
-- monochrome 0 = minBound
-- monochrome 1 = maxBound

monochrome :: Bit -> (Unsigned 8, Unsigned 8, Unsigned 8)
monochrome 0 = (0x40, 0x40, 0x40)
monochrome 1 = (0x20, 0xf0, 0x20)

makeTopEntity 'topEntity
