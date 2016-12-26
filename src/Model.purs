module Model where

type IdeaId = Int
type Idea = { title :: String, body :: String }

initialIdea :: Idea
initialIdea = { title: "title", body: "body" }

type List =
  {ideas :: Array IdeaId
  , nextId :: IdeaId
  }

initialList :: List
initialList =
  { ideas: [0]
  , nextId: 1
  }
