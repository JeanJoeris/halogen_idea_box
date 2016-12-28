module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)

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
import Data.Foreign (Foreign)
import Data.Foreign.Class (read)

import Network.HTTP.Affjax (AJAX, post, get)


import ListComponent (listComponent, ListQuery', initialListState, ListState', ListSlot (..), insertIdea)
import CreateIdeaComponent (createIdeaComponent, CreateIdeaSlot(..), CreateIdeaState, CreateIdeaQuery (..))
import Model as M

type AppEffects eff = H.HalogenEffects (ajax :: AJAX | eff)

type ChildState g = Either (ListState' g) CreateIdeaState
type ChildQuery = Coproduct ListQuery' CreateIdeaQuery
type ChildSlot = Either ListSlot CreateIdeaSlot

childPathList :: forall g. ChildPath (ListState' g) (ChildState g) ListQuery' ChildQuery ListSlot ChildSlot
childPathList = cpL

childPathCreateIdea :: forall g. ChildPath CreateIdeaState (ChildState g) CreateIdeaQuery ChildQuery CreateIdeaSlot ChildSlot
childPathCreateIdea = cpR

data Query a = LoadIdeas a
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)

type State = { on :: Boolean }
type State' g = ParentState State (ChildState g) Query ChildQuery g ChildSlot

initialState :: forall g. State' g
initialState = parentState { on: false }


-- ui :: forall g. H.Component State Query g
ui :: forall eff. Functor (Aff (AppEffects eff)) => H.Component (State' (Aff (AppEffects eff))) Query' (Aff (AppEffects eff))
ui = H.parentComponent { render, eval, peek: Just peek }
  where

  peek :: forall x. ChildF ChildSlot ChildQuery x -> H.ParentDSL State (ChildState (Aff (AppEffects eff))) Query ChildQuery (Aff (AppEffects eff)) ChildSlot Unit
  peek (ChildF p q) = case q of
    Coproduct (Right (CreateIdea _)) ->  do
      idea <- H.query' childPathCreateIdea CreateIdeaSlot (H.request GetIdea)
      case idea of
        Just idea' -> do
          H.query' childPathList ListSlot (H.action (insertIdea idea'))
          pure unit
        Nothing -> pure unit
    _ -> pure unit

  render :: State -> H.ParentHTML (ChildState (Aff (AppEffects eff))) Query ChildQuery (Aff (AppEffects eff)) ChildSlot
  render state =
    HH.div_
      [ HH.h1_
        [ HH.text "Haskell Idea Box!" ]
      , HH.button
        [ HE.onClick (HE.input_ LoadIdeas) ]
        [ HH.text "Load Ideas" ]
      , HH.slot' childPathCreateIdea CreateIdeaSlot (\_ -> { component: createIdeaComponent, initialState: M.initialIdea})
      , HH.slot' childPathList ListSlot (\_ -> { component: listComponent, initialState: initialListState})
      ]

  eval :: Query ~> H.ParentDSL State (ChildState (Aff (AppEffects eff))) Query ChildQuery (Aff (AppEffects eff)) ChildSlot
  eval (LoadIdeas next) = do
    -- H.modify (\state -> { on: not state.on })
    result <- H.fromAff (fetchJS)
    pure next

  fetchJS :: forall eff. Aff (ajax :: AJAX | eff) Unit
  fetchJS = do
    result <- get "http://localhost:2500/api/v1/idea"
    -- traceAnyA (result.response :: Foreign)
    case runExcept $ read result.response of
      Left _ -> traceA "Error"
      Right (val :: Array M.ServerIdea) -> traceAnyA val

    -- let response = result.response
    -- pure case runExcept $ readProp "js" response <|> readProp "error" response of
    --   Right js -> js
    --   Left _ -> "Invalid response"

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
