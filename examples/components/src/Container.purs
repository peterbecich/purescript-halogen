module Container where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Button as Button
import HelloGoodbye as HelloGoodbye

data Query a
  = HandleButton Button.Message a
  | CheckButtonState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

-- data Slot = ButtonSlot
type ButtonSlot = Either Unit Unit

-- derive instance eqButtonSlot :: Eq Slot
-- derive instance ordButtonSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing }

  render :: State -> H.ParentHTML Query Button.Query ButtonSlot m
  render state =
    HH.div_
      [ HH.slot' Left Button.myButton unit (HE.input HandleButton)
      , HH.p_
          [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
      , HH.p_
          [ HH.text
              $ "Last time I checked, the button was: "
              <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
              <> ". "
          , HH.button
              [ HE.onClick (HE.input_ CheckButtonState) ]
              [ HH.text "Check now" ]
          , HH.text "hello"
          ]
          -- see multitype components
      -- , HH.slot' ButtonSlot HelloGoodbye.helloGoodbye unit absurd
      -- , HH.p_ [HH.text "hello again"]
      -- , HH.p_ [HH.text "hello again"]
      -- , HH.p_ [HH.text "hello again"]
      -- , HH.p_ [HH.text "hello again"]
      -- , HH.p_ [HH.text "hello again"]
      -- , HH.p_ [HH.text "hello again"]

      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query ButtonSlot Void m
  eval (HandleButton (Button.Toggled _) next) = do
    _ <- H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
    pure next
  eval (CheckButtonState next) = do
    buttonState <- H.query Left $ H.request Button.IsOn
    _ <- H.modify (_ { buttonState = buttonState })
    pure next


      
