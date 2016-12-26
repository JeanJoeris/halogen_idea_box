module ListComponent where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Core (className)
import Halogen.Component (parentState, ParentState, ChildF (..))
import Data.Maybe (Maybe (..))
import Data.Functor.Coproduct (Coproduct (..))
import Data.Array ((:), filter)
import Data.Either (Either (..))
import Debug.Trace (traceA)

import IdeaComponent (ideaComponent, IdeaQuery (..))
import Model as M

data ListQuery a = InsertIdea M.Idea a
type ListQuery' = Coproduct ListQuery (ChildF Slot IdeaQuery)

data ListSlot = ListSlot
derive instance eqListSlot :: Eq ListSlot
derive instance ordListSlot :: Ord ListSlot

data Slot = IdeaSlot Int
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
-- instance itemSlotEq :: Eq ItemSlot where
--   eq ItemSlot ItemSlot = true
--
-- instance itemSlotOrd :: Ord ItemSlot where
--   compare ItemSlot ItemSlot = EQ
type ListState = M.List
type ListState' g = ParentState ListState M.Idea ListQuery IdeaQuery g Slot

insertIdea :: forall a. M.Idea -> a -> ListQuery' a
insertIdea idea next = Coproduct (Left (InsertIdea idea next) )

initialListState :: forall g. ListState' g
initialListState = parentState M.initialList


-- ui :: forall g. H.Component M.List Query g
listComponent :: forall g. Functor g => H.Component (ListState' g) ListQuery' g
listComponent = H.parentComponent { render, eval, peek: Just peek }
  where

  peek :: forall x. ChildF Slot IdeaQuery x -> H.ParentDSL ListState M.Idea ListQuery IdeaQuery g Slot Unit
  peek (ChildF p q) = case q of
    DeleteIdea _ ->
      case p of
        IdeaSlot id -> do
          H.modify (\state -> state {ideas = (filter (\ideaId -> ideaId /= id) state.ideas)})
    _ -> pure unit
    -- Coproduct (Right (CreateIdea _)) ->  do
    --   idea <- H.query' childPathCreateIdea CreateIdeaSlot (H.request GetIdea)
    --   case idea of
    --     Just idea' -> do
    --       H.query' childPathList ListSlot (H.action (insertIdea idea'))
    --       pure unit
    --     Nothing -> pure unit
    -- _ -> pure unit

  render :: M.List -> H.ParentHTML M.Idea ListQuery IdeaQuery g Slot
  render state =
    HH.div_
      [
      HH.div [HP.class_ (className "idea-list")]
        (map (renderIdea state.nextIdea) state.ideas)
      ]

  renderIdea :: M.Idea -> M.IdeaId -> H.ParentHTML M.Idea ListQuery IdeaQuery g Slot
  renderIdea idea ideaId =
    HH.slot (IdeaSlot ideaId) (\_ -> { component: ideaComponent, initialState: idea})


  eval :: ListQuery ~> H.ParentDSL M.List M.Idea ListQuery IdeaQuery g Slot

  eval (InsertIdea idea next) = do
    H.modify (\state -> { nextId: state.nextId + 1, nextIdea: idea, ideas: (state.nextId : state.ideas)})
    pure next
