{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module IcfpcMmxx.Types.NewtypeWrappers
     ( ProblemId    (..)
     , ApiKey       (..)
     , DeployKey    (..)
     , LogKey       (..)
     , SubmissionId (..)
     , TeamId       (..)
     , CommitHash   (..)
     , BranchName   (..)
     , Score        (..)
     , ResponseId   (..)
     ) where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)

import Servant.API (ToHttpApiData)


-- | This is just a text in the Swagger spec.
newtype ProblemId = ProblemId { fromProblemId :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

-- | This is just a text in the Swagger spec.
newtype ApiKey = ApiKey { fromApiKey :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | This is just a text in the Swagger spec.
newtype DeployKey = DeployKey { fromDeployKey :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | This is just a text in the Swagger spec.
newtype LogKey = LogKey { fromLogKey :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

newtype SubmissionId = SubmissionId { fromSubmissionId :: Int32 }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

newtype TeamId = TeamId { fromTeamId :: UUID }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | This is just a text in the Swagger spec.
newtype CommitHash = CommitHash { fromCommitHash :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

-- | This is just a text in the Swagger spec.
newtype BranchName = BranchName { fromBranchName :: Text }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

-- | Could this value be negative?
--   I’d use @Word@ but it’s @Int64@ in the Swagger spec so be it.
newtype Score = Score { fromScore :: Int64 }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

newtype ResponseId = ResponseId { fromResponseId :: UUID }
  deriving stock   (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
