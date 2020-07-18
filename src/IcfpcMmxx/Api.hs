{-# LANGUAGE DataKinds, TypeOperators, PatternSynonyms #-}

module IcfpcMmxx.Api
     ( ContestApi
     , AliensApi
     , LogsApi
     , ScoreboardApi
     , SubmissionApi
     , TeamsApi
     ) where

import Data.Text (Text)

import Servant.API

import IcfpcMmxx.Types.ScoreboardApi  (LightningScoreboardDto)
import IcfpcMmxx.Types.SubmissionsApi (SubmissionDto)
import IcfpcMmxx.Types.TeamsApi       (RegisteredTeamDto)

import IcfpcMmxx.Types.NewtypeWrappers (LogKey, SubmissionId, ResponseId)


-- | This should comply with this Swagger scheme:
--   https://icfpc2020-api.testkontur.ru/swagger/index.html
type  ContestApi
   =  AliensApi
 :<|> LogsApi
 :<|> ScoreboardApi
 :<|> SubmissionApi
 :<|> TeamsApi

-- | Response type was not provided for this API section in the Swagger scheme!
--
-- Response from aliens is probably a secret we have to figure out.
-- TODO Change type from "NoContent" to a type of aliens response when figure out.
--
-- What to send to aliens is probably a secret we have to figure out.
-- In the spec it's text/plain.
-- TODO Change type from "Text" to a type of message for aliens when figure out.
--
-- N.B. Keep in mind a story about 10 seconds! Maybe you have to send something
-- first and wait for 10 seconds to request a response.
type AliensApi =
  "aliens" :>
      (  Capture "responseId" ResponseId :> Get '[JSON] NoContent
    :<|> "send" :> ReqBody '[PlainText] Text :> Post '[JSON] NoContent
      )

-- | Response type was not provided for this API section in the Swagger scheme!
--
-- TODO Figure out what the “logKey” is.
-- TODO Figure about what the response is.
type LogsApi
   = "logs"
  :> QueryParam' '[Required, Strict] "logKey" LogKey
  :> Get '[JSON] NoContent

type ScoreboardApi =
  "scoreboard" :> "lightning" :> Get '[JSON] LightningScoreboardDto

type SubmissionApi =
  "submissions" :>
    (  Get '[JSON] [SubmissionDto]
  :<|> Capture "submissionId" SubmissionId :> Get '[JSON] SubmissionDto
    )

type TeamsApi =
  "teams" :> "current" :> Get '[JSON] RegisteredTeamDto
