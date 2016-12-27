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
                 | DecreaseQuality a
                 | IncreaseQuality a

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
      , HH.p [HP.class_ (className "idea-quality")]
        [HH.text (show state.quality)]
      , HH.button
        [ HE.onClick (HE.input_ DeleteIdea) ]
        [ HH.text "Delete me!"]
      , HH.button
        [ HE.onClick (HE.input_ DecreaseQuality) ]
        [ HH.text "-"]
      , HH.button
        [ HE.onClick (HE.input_ IncreaseQuality) ]
        [ HH.text "+"]
      ]

  eval :: IdeaQuery ~> H.ComponentDSL M.Idea IdeaQuery g
  eval (DeleteIdea next) = do
    pure next

  eval (DecreaseQuality next) = do
    H.modify (\state -> case state.quality of
      M.Swill -> state
      M.Plausible -> state { quality = M.Swill }
      M.Genius -> state { quality = M.Plausible }
    )
    pure next

  eval (IncreaseQuality next) = do
    H.modify (\state -> case state.quality of
      M.Swill -> state { quality = M.Plausible}
      M.Plausible -> state { quality = M.Genius }
      M.Genius -> state
    )
    pure next
