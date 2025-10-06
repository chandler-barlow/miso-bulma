{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_app (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "app"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A miso-sampler application"
copyright :: String
copyright = "haskell-miso @ 2025"
homepage :: String
homepage = ""
