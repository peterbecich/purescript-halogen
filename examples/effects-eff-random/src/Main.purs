module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Control.Monad.Eff.Random (RANDOM)
import Component (ui)

main :: Eff (HA.HalogenEffects (random :: RANDOM, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
