{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

-- | Submissions API
--
-- Data-types names here match ones from the Swagger schema:
-- https://icfpc2020-api.testkontur.ru/swagger/index.html
module IcfpcMmxx.Types.SubmissionsApi
     ( SubmissionDto (..)
     , SubmissionStatus (..)
     ) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Time.Clock (UTCTime)

import IcfpcMmxx.Utils
import IcfpcMmxx.Types.NewtypeWrappers (SubmissionId, LogKey, CommitHash, BranchName)


data SubmissionDto
   = SubmissionDto
   { submissionId :: SubmissionId
   , platform :: Maybe Text
   , createdAt :: UTCTime
   , active :: Bool
   , commitHash :: Maybe CommitHash
   , commitMessage :: Maybe Text
   , branchName :: Maybe BranchName
   , status :: SubmissionStatus

   , buildLogKey :: Maybe LogKey -- ^ I assumed it’s "LogKey"
   , testLogKey  :: Maybe LogKey -- ^ I assumed it’s "LogKey"

   } deriving stock    (Eq, Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

data SubmissionStatus
   = Building
   | Testing
   | Failed_Build
   | Failed_Test
   | Failed_Unknown
   | Succeeded
     deriving stock (Show, Eq, Enum, Bounded, Typeable)

instance ToJSON   SubmissionStatus where toJSON    = stringyEnumToJSON
instance FromJSON SubmissionStatus where parseJSON = stringyEnumFromJSON


-- Example for "SubmissionDto" from Swagger schema
{-
  "submissionId": 0,
  "platform": "string",
  "createdAt": "2020-07-17T23:15:56.085Z",
  "active": true,
  "commitHash": "string",
  "commitMessage": "string",
  "branchName": "string",
  "status": "Building",
  "buildLogKey": "string",
  "testLogKey": "string"
-}
