module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container as Container
import Control.Monad.Eff.Console (CONSOLE, error, log)

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
