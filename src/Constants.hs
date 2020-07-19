module Constants
     ( localBaseUrl
     , defaultGalaxyFilePath
     ) where

import Newtypes


localBaseUrl :: Either String BaseUrl
localBaseUrl = parseBaseUrl "https://icfpc2020-api.testkontur.ru"


defaultGalaxyFilePath :: FilePath
defaultGalaxyFilePath = "data/galaxy.txt"
