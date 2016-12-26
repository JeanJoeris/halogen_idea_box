module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.Component (parentState, ParentState, ChildF (..))
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Functor.Coproduct (Coproduct (..))
import Debug.Trace (traceA, traceAnyA)

import ListComponent (listComponent, ListQuery', initialListState, ListState', ListSlot (..), insertIdea)
import CreateIdeaComponent (createIdeaComponent, CreateIdeaSlot(..), CreateIdeaState, CreateIdeaQuery (..))
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
ui = H.parentComponent { render, eval, peek: Just peek }
  where

  peek :: forall x. ChildF ChildSlot ChildQuery x -> H.ParentDSL State (ChildState g) Query ChildQuery g ChildSlot Unit
  peek (ChildF p q) = case q of
    Coproduct (Right (CreateIdea _)) ->  do
      idea <- H.query' childPathCreateIdea CreateIdeaSlot (H.request GetIdea)
      case idea of
        Just idea' -> do
          H.query' childPathList ListSlot (H.action (insertIdea idea'))
          pure unit
        Nothing -> pure unit
    -- Remove _ -> do
    --   wasComplete <- query p (request IsCompleted)
    --   when (fromMaybe false wasComplete) $ modify $ updateNumCompleted (`sub` 1)
      -- modify (removeTask p)
    -- ToggleCompleted b _ -> modify $ updateNumCompleted (if b then (+ 1) else (`sub` 1))
    _ -> pure unit

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
