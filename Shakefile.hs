{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude hiding (lift)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Trans.Class

clashProject = ClashProject
    { projectName = "FrameBuffer"
    , clashModule = "FrameBuffer"
    , clashTopName = "topEntity"
    , topName = "Top"
    , clashFlags =
        [ "-iretroclash-lib/src"
        , "-Wno-partial-type-signatures"
        ]
    , buildDir = "_build"
    , clashDir = "clash-syn"
    }

main :: IO ()
main = clashShake clashProject $ do
    ClashProject{..} <- ask
    let synDir = buildDir </> clashDir

    kit@ClashKit{..} <- clashRules Verilog "src" $ do
        need [ buildDir </> "seabios8x8.rom" ]

    -- xilinxISE kit papilioPro "target/papilio-pro" "papilio-pro"
    -- xilinxISE kit papilioOne "target/papilio-one" "papilio-one"
    xilinxVivado kit nexysA750T "target/nexys-a7-50t" "nexys-a7-50t"

    lift $ do
      buildDir </> "seabios8x8.rom" %> binImage (Just $ 256 * 8) "seabios8x8.bin"
