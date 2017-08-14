module Button where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.String

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

-- data Component (h :: Type -> Type -> Type) (f :: Type -> Type) i o (m :: Type -> Type)

-- The "public" type for a component, with details of the component internals existentially hidden.

--     h is the type that will be rendered by the component, usually HTML
--     f is the query algebra
--     i is the input value type that will be mapped to an f whenever the parent of this component renders
--     o is the type for the component's output messages
--     m is the monad used for non-component-state effects

-- h = HTML
-- f = Query
-- i = Unit
-- o = Message
-- m = free??

-- type ComponentSpec h s f i o m = { initialState :: i -> s, render :: s -> h Void (f Unit), eval :: f ~> (ComponentDSL s f o m), receiver :: i -> Maybe (f Unit) }

-- A spec for a component with no possible children.
--     h is the type that will be rendered by the component, usually HTML
--     s is the component's state
--     f is the query algebra
--     i is the input value type that will be mapped to an f whenever the parent of this component renders
--     o is the type for the component's output messages
--     m is the monad used for non-component-state effects

myButton :: forall m. H.Component HH.HTML Query Unit Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      -- https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen#v:put
      _ <- H.put nextState
      -- https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen#v:raise
      _ <- H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)


data ThreeState = One | Two | Three

data ThreeStateQuery a =
  GotoOne a | GotoTwo a | GotoThree a | GotoNextState a | GetState (ThreeState -> a)

-- data ThreeStateMessage = Hello | Goodbye
data ThreeStateMessage = Hello ThreeState

threeStateSwitch :: forall m. H.Component HH.HTML ThreeStateQuery Unit ThreeStateMessage m
threeStateSwitch =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: ThreeState
    initialState = One

    render :: ThreeState -> H.ComponentHTML ThreeStateQuery
    render One = HH.button [ HP.title "one", HE.onClick (HE.input_ GotoNextState) ] [HH.text "one"]
    render Two = HH.button [ HP.title "two", HE.onClick (HE.input_ GotoNextState)] [HH.text "two"]
    render Three = HH.button [ HP.title "two", HE.onClick (HE.input_ GotoNextState)] [HH.text "three"]

    eval :: forall m. ThreeStateQuery ~> H.ComponentDSL ThreeState ThreeStateQuery ThreeStateMessage m
    eval (GotoOne x) = pure x
    eval (GotoTwo x) = pure x
    eval (GotoThree x) = pure x
    eval (GotoNextState x) = do
      s <- H.get
      case s of
        One -> do
          _ <- H.put Two
          pure x
        Two -> do
          _ <- H.put Three
          pure x
        Three -> do
          _ <- H.put One
          pure x

    eval (GetState probe) = do
      s <- H.get
      _ <- H.raise $ Hello Two
      pure (probe s)


-- eval _ = do
    --   s <- H.get
    --   _ <- H.raise $ Hello true
    --   pure (reply s)

-- data UserInputQuery a = Add

-- userInput :: forall m. H.Component HH.HTML 
    
