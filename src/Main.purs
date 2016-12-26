module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.Component (parentState, ParentState, ChildF)
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Data.Maybe (Maybe (Nothing))
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)

import ListComponent (listComponent, ListQuery', initialListState, ListState', ListSlot(..))
import CreateIdeaComponent (createIdeaComponent, CreateIdeaSlot(..), CreateIdeaState, CreateIdeaQuery)
import Model as M


type ChildState g = Either (ListState' g) CreateIdeaState
type ChildQuery = Coproduct ListQuery' CreateIdeaQuery
type ChildSlot = Either ListSlot CreateIdeaSlot

childPathList :: forall g. ChildPath (ListState' g) (ChildState g) ListQuery' ChildQuery ListSlot ChildSlot
childPathList = cpL

childPathCreateIdea :: forall g. ChildPath CreateIdeaState (ChildState g) CreateIdeaQuery ChildQuery CreateIdeaSlot ChildSlot
childPathCreateIdea = cpR

data Query a = ToggleState a
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)

type State = { on :: Boolean }
type State' g = ParentState State (ChildState g) Query ChildQuery g ChildSlot

initialState :: forall g. State' g
initialState = parentState { on: false }


-- ui :: forall g. H.Component State Query g
ui :: forall g. Functor g => H.Component (State' g) Query' g
ui = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> H.ParentHTML (ChildState g) Query ChildQuery g ChildSlot
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Haskell Idea Box!" ]
      , HH.slot' childPathCreateIdea CreateIdeaSlot (\_ -> { component: createIdeaComponent, initialState: M.initialIdea})
      , HH.slot' childPathList ListSlot (\_ -> { component: listComponent, initialState: initialListState})
      ]

  eval :: Query ~> H.ParentDSL State (ChildState g) Query ChildQuery g ChildSlot
  eval (ToggleState next) = do
    -- H.modify (\state -> { on: not state.on })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
