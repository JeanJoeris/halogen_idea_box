module IdeaComponent where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Core (Prop, prop, propName, attrName, className)
import Data.Maybe (Maybe (Just))

import Model as M

data IdeaQuery a = ToggleState a

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
      ]

  eval :: IdeaQuery ~> H.ComponentDSL M.Idea IdeaQuery g
  eval (ToggleState next) = do
    -- H.modify (\state -> { on: not state.on })
    pure next
