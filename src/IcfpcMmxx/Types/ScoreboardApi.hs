{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Scoreboard API
--
-- Data-types names here match ones from the Swagger schema:
-- https://icfpc2020-api.testkontur.ru/swagger/index.html
module IcfpcMmxx.Types.ScoreboardApi
     ( LightningScoreboardDto (..)
     , ProblemDescriptionDto (..)
     , TeamForProblemsScoreboardDto (..)
     , ProblemDto (..)
     , TeamDto (..)
     ) where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime)
import Data.HashMap.Strict (HashMap)

import IcfpcMmxx.Utils
import IcfpcMmxx.Types.NewtypeWrappers (ProblemId, TeamId, Score)


data LightningScoreboardDto
   = LightningScoreboardDto
   { frozenAt :: Maybe UTCTime
   , problems :: [ProblemDescriptionDto]
   , teams :: [TeamForProblemsScoreboardDto]
   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data ProblemDescriptionDto
   = ProblemDescriptionDto
   { problemId :: Maybe ProblemId
   -- ^ Weird but this field is “nullable” in the Swagger spec 🤷

   , description :: Maybe Text
   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data TeamForProblemsScoreboardDto
   = TeamForProblemsScoreboardDto
   { team :: TeamDto
   , score :: Score
   , solvedAt :: UTCTime
   , problems :: HashMap ProblemId ProblemDto
   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data ProblemDto
   = ProblemDto
   { score :: Score
   , solvedAt :: UTCTime
   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data TeamDto
   = TeamDto
   { teamId :: TeamId
   , teamName :: Maybe Text

   , customData :: HashMap Text Text
   -- ^ This described as @HashMap Text Text@ in the spec.
   --   But I realized all the records have one single field “country”.
   --   E.g. “RUS”, “JPN”, “World”, etc.

   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)


-- Example for "LightningScoreboardDto" from Swagger schema
{-
  "frozenAt": "2020-07-17T19:34:58.501Z",
  "problems": [
    {
      "problemId": "string",
      "description": "string"
    }
  ],
  "teams": [
    {
      "team": {
        "teamId": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
        "teamName": "string",
        "customData": {
          "additionalProp1": "string",
          "additionalProp2": "string",
          "additionalProp3": "string"
        }
      },
      "score": 0,
      "solvedAt": "2020-07-17T19:34:58.501Z",
      "problems": {
        "additionalProp1": {
          "score": 0,
          "solvedAt": "2020-07-17T19:34:58.501Z"
        },
        "additionalProp2": {
          "score": 0,
          "solvedAt": "2020-07-17T19:34:58.501Z"
        },
        "additionalProp3": {
          "score": 0,
          "solvedAt": "2020-07-17T19:34:58.501Z"
        }
      }
    }
  ]
-}
