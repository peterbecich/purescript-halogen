module HelloGoodbye where

import Prelude
import Halogen as H
import Halogen.HTML as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))


data State = Hello | Goodbye

-- derive instance showState :: Show State
instance showState :: Show State where
  show Hello = "Hello!"
  show Goodbye = "Goodbye!"

data Query a = Toggle a | SayHello a | SayGoodbye a

helloGoodbye :: forall m. H.Component HH.HTML Query Unit Void m
helloGoodbye =
  H.component
    { initialState: const Hello
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render Hello = HH.p_ [ HH.text $ show Hello ]
    render Goodbye = HH.p_ [ HH.text $ show Goodbye ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Toggle next) = do
      currentState <- H.get
      _ <- case currentState of
        Hello -> H.put Goodbye
        Goodbye -> H.put Hello
      pure next
    eval (SayHello next) = do
      _ <- H.put Goodbye
      pure next
    eval (SayGoodbye next) = do
      _ <- H.put Hello
      pure next
      
        

