module Model where

import Prelude

type IdeaId = Int
type Idea = { title :: String, body :: String, quality :: Quality }

data Quality = Swill | Plausible | Genius
derive instance eqQuality :: Eq Quality
instance showQuality :: Show Quality where
  show Swill = "swill"
  show Plausible = "plausible"
  show Genius = "genius"
instance ordQuality :: Ord Quality where
  compare Swill Swill = EQ
  compare Plausible Plausible = EQ
  compare Genius Genius = EQ
  compare Swill _ = LT
  compare _ Swill = GT
  compare Genius _ = GT
  compare _ Genius = LT


initialIdea :: Idea
initialIdea = { title: "title", body: "body", quality: Swill}

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
