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
topEntity = withEnableGen $ let
    board = vga
      where
        (frameEnd, vga) = video write

        frameEnd' = riseEvery @Dom25 (SNat @100_000)
        ptr = regEn (0 :: Index (15 * 10)) frameEnd' $ satAdd SatWrap 23 <$> ptr
        char = regEn (0 :: Unsigned 8) frameEnd' $ char + 1
        write = packWrite <$> ptr <*> (Just <$> char)

    in board

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
    => Signal Dom25 (Maybe (Index (15 * 10), Unsigned 8))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (matchDelay rgb False frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver @Dom25 vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale (SNat @5) . center @600 $ vgaX
    vgaY' = scale (SNat @5) . center @400 $ vgaY

    rgb = maybe <$> delayI undefined grid <*> pure monochrome <*> fontPixel

    cellX = fmap (fromIntegral @_ @(Index 15) . (`shiftR` 3)) <$> vgaX'
    cellY = fmap (fromIntegral @_ @(Index 10) . (`shiftR` 3)) <$> vgaY'

    glyphX = fmap (fromIntegral @_ @(Unsigned 3)) <$> vgaX'
    glyphY = fmap (fromIntegral @_ @(Unsigned 3)) <$> vgaY'

    newCell = cellX ./=. register Nothing cellX
    newPixel = glyphX ./=. register Nothing glyphX

    char = frameBuffer (0 :: Unsigned 8) write cellX cellY
    fontAddr = maybe 0 bitCoerce <$> (liftA2 (,) <$> char <*> (delayI Nothing $ fromSignal glyphY))
    loadRow = delayedRom (romFilePow2 @11 @8 "seabios8x8.rom") fontAddr

    row = unsafeFromSignal @_ @_ @3 . register (0 :: BitVector 8) . toSignal $
        mux (delayI False . fromSignal $ newCell) loadRow $
        mux (delayI False . fromSignal $ newPixel) ((`shiftL` 1) <$> antiDelay d1 row) $
        antiDelay d1 row

    fontPixel = enable (delayI False . fromSignal $ isJust <$> cellX .&&. isJust <$> cellY) $
        msb <$> row

    grid = fromSignal $ mux parity (pure red) (pure green)
    parity = (maybe 0 lsb <$> vgaX) .==. (maybe 0 lsb <$> vgaY)
    red = (0xff, 0x00, 0x00) :: (Unsigned 8, Unsigned 8, Unsigned 8)
    green = (0x00, 0xff5, 0x00) :: (Unsigned 8, Unsigned 8, Unsigned 8)

monochrome :: Bit -> (Unsigned 8, Unsigned 8, Unsigned 8)
monochrome 0 = (0x40, 0x40, 0x40)
monochrome 1 = (0x20, 0xf0, 0x20)

makeTopEntity 'topEntity
