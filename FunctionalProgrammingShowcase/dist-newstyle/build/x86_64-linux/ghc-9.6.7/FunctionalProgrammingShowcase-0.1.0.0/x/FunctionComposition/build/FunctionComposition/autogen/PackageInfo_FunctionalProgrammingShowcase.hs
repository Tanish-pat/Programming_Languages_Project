{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_FunctionalProgrammingShowcase (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "FunctionalProgrammingShowcase"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Showcasing functional programming concepts"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
