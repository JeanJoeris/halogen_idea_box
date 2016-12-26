module CreateIdeaComponent where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Core (Prop, prop, propName, attrName, className)
import Data.Maybe (Maybe (Just))

import Model as M

data CreateIdeaSlot = CreateIdeaSlot
derive instance eqCreateIdeaSlot :: Eq CreateIdeaSlot
derive instance ordCreateIdeaSlot :: Ord CreateIdeaSlot

data CreateIdeaQuery a = CreateIdea a

type CreateIdeaState = M.Idea

createIdeaComponent :: forall g. H.Component CreateIdeaState CreateIdeaQuery g
createIdeaComponent = H.component { render, eval }
  where

  render :: CreateIdeaState -> H.ComponentHTML CreateIdeaQuery
  render state =
    HH.div [HP.class_ (className "create-idea")]
    [ HH.div [HP.class_ (className "create-idea-title")]
      [ HH.input
        [ HP.inputType HP.InputText
        , HP.placeholder "Idea Title"
        , HP.value state.title
        ]
      ]
    , HH.div [HP.class_ (className "create-idea-body")]
      [ HH.textarea
        [ HP.placeholder "Idea Body"
        , HP.value state.body
        ]
      ]
    , HH.button
      [ HE.onClick (HE.input_ CreateIdea) ]
      [HH.text "Create an Idea"]
    ]

  eval :: CreateIdeaQuery ~> H.ComponentDSL CreateIdeaState CreateIdeaQuery g
  eval (CreateIdea next) = do
    -- H.modify (\state -> { on: not state.on })
    pure next
