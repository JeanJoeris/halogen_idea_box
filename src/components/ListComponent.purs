module ListComponent where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Core (className)
import Halogen.Component (parentState, ParentState, ChildF)
import Data.Maybe (Maybe (Nothing))
import Data.Functor.Coproduct (Coproduct)

import IdeaComponent (ideaComponent, IdeaQuery)
import Model as M

data ListQuery a = ToggleState a
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

-- type M.List = { on :: Boolean }
type ListState' g = ParentState M.List M.Idea ListQuery IdeaQuery g Slot

initialListState :: forall g. ListState' g
initialListState = parentState M.initialList


-- ui :: forall g. H.Component M.List Query g
listComponent :: forall g. Functor g => H.Component (ListState' g) ListQuery' g
listComponent = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: M.List -> H.ParentHTML M.Idea ListQuery IdeaQuery g Slot
  render state =
    HH.div_
      [
      HH.div [HP.class_ (className "idea-list")]
        (map renderIdea state.ideas)
        -- [ HH.slot (IdeaSlot 0) (\_ -> { component: ideaComponent, initialListState: M.initialIdea})
        -- , HH.slot (IdeaSlot 1) (\_ -> { component: ideaComponent, initialListState: M.initialIdea})]
      ]

  renderIdea :: M.IdeaId -> H.ParentHTML M.Idea ListQuery IdeaQuery g Slot
  renderIdea ideaId =
    HH.slot (IdeaSlot ideaId) (\_ -> { component: ideaComponent, initialState: M.initialIdea})


  eval :: ListQuery ~> H.ParentDSL M.List M.Idea ListQuery IdeaQuery g Slot
  eval (ToggleState next) = do
    -- H.modify (\state -> { on: not state.on })
    pure next
