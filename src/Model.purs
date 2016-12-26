module Model where

import Prelude

type IdeaId = Int
type Idea = { title :: String, body :: String }
-- instance showIdea :: Show Idea where
--   show idea = "{ title: " <> idea.title  <> ", body: " <> idea.body <> " }"

initialIdea :: Idea
initialIdea = { title: "title", body: "body" }

type List =
  {ideas :: Array IdeaId
  , nextId :: IdeaId
  , nextIdea :: Idea
  }

initialList :: List
initialList =
  { ideas: [0]
  , nextId: 1
  , nextIdea: initialIdea
  }
