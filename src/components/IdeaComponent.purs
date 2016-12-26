module IdeaComponent where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Core (Prop, prop, propName, attrName, className)
import Data.Maybe (Maybe (Just))
import Debug.Trace (traceA)

import Model as M

data IdeaQuery a = DeleteIdea a

-- contentEditable :: forall i. Boolean -> Prop i
-- contentEditable = prop (propName "contenteditable")(Just $ attrName "contenteditable")

ideaComponent :: forall g. H.Component M.Idea IdeaQuery g
ideaComponent = H.component { render, eval }
  where

  render :: M.Idea -> H.ComponentHTML IdeaQuery
  render state =
    HH.div [HP.class_ (className "idea")]
      -- [ HH.p [contentEditable true, HP.class_ "idea-title"]
      [ HH.p [HP.class_ (className "idea-title")]
        [HH.text state.title]
      -- , HH.p [contentEditable true, HP.class_ "idea-body"]
      , HH.p [HP.class_ (className "idea-body")]
        [HH.text state.body]
      , HH.button
        [ HE.onClick (HE.input_ DeleteIdea) ]
        [ HH.text "Delete me!"]
      ]

  eval :: IdeaQuery ~> H.ComponentDSL M.Idea IdeaQuery g
  eval (DeleteIdea next) = do
    pure next
