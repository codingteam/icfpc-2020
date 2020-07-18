{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Teams API
--
-- Data-types names here match ones from the Swagger schema:
-- https://icfpc2020-api.testkontur.ru/swagger/index.html
module IcfpcMmxx.Types.TeamsApi
     ( RegisteredTeamDto (..)
     , BuildSystemRegistrationInfoDto (..)
     , BuildSystemRegistrationStatus (..)
     ) where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Data.HashMap.Strict (HashMap)
import Data.UUID (UUID)
import Data.Typeable (Typeable)

import IcfpcMmxx.Utils
import IcfpcMmxx.Types.NewtypeWrappers (ApiKey, DeployKey, TeamId)


data RegisteredTeamDto
   = RegisteredTeamDto
   { teamId :: TeamId
   , teamName :: Maybe Text
   , apiKey :: Maybe ApiKey
   , deployKey :: Maybe DeployKey
   , buildSystemRegistrationInfo :: BuildSystemRegistrationInfoDto
   , gitRepositoryUrl :: Maybe Text

   , customData :: HashMap Text Text
   -- ^ See comment for "customData" of "TeamDto".
   --   Maybe the same could be applied to this.

   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data BuildSystemRegistrationInfoDto
   = BuildSystemRegistrationInfoDto
   { status :: BuildSystemRegistrationStatus
   , errorMessage :: Maybe Text
   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

-- | Stringy enum
data BuildSystemRegistrationStatus
   = Pending
   | Failed_GitRepository_InvalidUrl
   | Failed_GitRepository_AuthFailed
   | Failed_GitRepository_NoBranch
   | Failed_Unknown
   | Succeeded
     deriving stock (Show, Eq, Enum, Bounded, Typeable)

instance ToJSON BuildSystemRegistrationStatus where
  toJSON = stringyEnumToJSON

instance FromJSON BuildSystemRegistrationStatus where
  parseJSON = stringyEnumFromJSON


-- Example for "RegisteredTeamDto" from Swagger schema
{-
  "teamId": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "teamName": "string",
  "apiKey": "string",
  "deployKey": "string",
  "buildSystemRegistrationInfo": {
    "status": "Pending",
    "errorMessage": "string"
  },
  "gitRepositoryUrl": "string",
  "customData": {
    "additionalProp1": "string",
    "additionalProp2": "string",
    "additionalProp3": "string"
  }
-}
