module Model where

import Prelude

import Data.Foreign (F, fail, ForeignError (..))
import Data.Foreign.Class (class IsForeign, readProp)

type IdeaId = Int
type Idea = { title :: String, body :: String, quality :: Quality }
newtype ServerIdea = ServerIdea { title :: String, body :: String, quality :: Quality, id :: IdeaId }
instance isForeignServerIdea :: IsForeign ServerIdea where
  read foreignIdea = do
    title <- readProp "title" foreignIdea
    body <- readProp "body" foreignIdea
    rawQuality <- readProp "quality" foreignIdea
    quality <- parseQuality rawQuality
    id <- readProp "id" foreignIdea
    pure (ServerIdea { title, body, quality, id})

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

parseQuality :: String -> F Quality
parseQuality "swill" = pure Swill
parseQuality "plausible" = pure Plausible
parseQuality "genius" = pure Genius
parseQuality _ = fail (ForeignError "Quality not found")

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
