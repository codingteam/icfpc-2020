{-# LANGUAGE TypeApplications #-}

-- | See main module for a usage example.
module IcfpcMmxx.Request
     ( -- * Aliens API
       getResponseFromAliens
     , sendMessageToAliens

     -- * Logs API
     , getLogs

     -- * Scoreboard API
     , getScoreboard

     -- * Submissions API
     , getSubmissions
     , getSubmissionById

     -- * Teams API
     , getCurrentTeam
     ) where

import Data.Text (Text)
import Data.Proxy (Proxy (Proxy))

import Servant.API
import Servant.Client

import IcfpcMmxx.Api (ContestApi)

import IcfpcMmxx.Types.ScoreboardApi  (LightningScoreboardDto)
import IcfpcMmxx.Types.SubmissionsApi (SubmissionDto)
import IcfpcMmxx.Types.TeamsApi       (RegisteredTeamDto)

import IcfpcMmxx.Types.NewtypeWrappers (LogKey, SubmissionId, ResponseId)


getResponseFromAliens :: ResponseId -> ClientM NoContent
sendMessageToAliens :: Text -> ClientM NoContent

getLogs :: LogKey -> ClientM NoContent

getScoreboard :: ClientM LightningScoreboardDto

getSubmissions :: ClientM [SubmissionDto]
getSubmissionById :: SubmissionId -> ClientM SubmissionDto

getCurrentTeam :: ClientM RegisteredTeamDto

(
  (
    getResponseFromAliens
    :<|>
    sendMessageToAliens
  )
  :<|>
  getLogs
  :<|>
  getScoreboard
  :<|>
  (
    getSubmissions
    :<|>
    getSubmissionById
  )
  :<|>
  getCurrentTeam

  ) = client (Proxy @ContestApi)
