module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception
import DOM (DOM)


import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Button as B

main :: Eff (HA.HalogenEffects (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM)) Unit
main = HA.runHalogenAff do
  -- _ <- log "Rendering Halogen component..."
  body <- HA.awaitBody
  button1 <- runUI B.myButton unit body
  button2 <- runUI B.myButton unit body
  button3 <- runUI B.myButton unit body
  button4 <- runUI B.myButton unit body
  button5 <- runUI B.myButton unit body
  -- _ <- button1.subscribe $ CR.consumer \(B.Toggled newState) -> do
  --   _ <- log $ "Button1 was toggeled to: " <> show newState
  --   pure Nothing
  -- _ <- button2.subscribe $ CR.consumer \(B.Toggled newState) -> do
  --   _ <- log $ "Button2 was toggeled to: " <> show newState
  --   pure Nothing
  -- _ <- button3.subscribe $ CR.consumer \(B.Toggled newState) -> do
  --   _ <- log $ "Button3 was toggeled to: " <> show newState
  --   pure Nothing
  -- _ <- button4.subscribe $ CR.consumer \(B.Toggled newState) -> do
  --   _ <- log $ "Button4 was toggeled to: " <> show newState
  --   pure Nothing
  -- _ <- button5.subscribe $ CR.consumer \(B.Toggled newState) -> do
  --   _ <- log $ "Button5 was toggeled to: " <> show newState
  --   pure Nothing
  runUI B.threeStateSwitch unit body
