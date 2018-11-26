{-# LANGUAGE OverloadedStrings #-}
module CircleCI where

import Config
import Types

-- import Network.CircleCI hiding (commit, number)
import Network.CircleCI.Build hiding (commit)
import Servant.Client

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.HashMap.Strict as HashMap

projectPoint :: Config -> ProjectPoint
projectPoint cfg = ProjectPoint (T.pack $ githubUser cfg) (T.pack $ githubProject cfg)

buildOnCircleCI :: Config -> BuildRequest -> IO (Either ServantError BuildInfo)
buildOnCircleCI cfg req = do
  tok <- T.readFile (circleTokenFile cfg)
  flip runCircleCI (AccountAPIToken tok) $
    triggerBuild (projectPoint cfg) opts

  where opts = TriggerBuildOptions (BuildRevision $ T.pack rev) env
        rev  = commit (source req)
        env = HashMap.fromList [("CIRCLE_JOB", job)]
        CircleCI job = jobType req

getStatusCircleCI :: Config -> BuildNumber -> IO (Either ServantError BuildInfo)
getStatusCircleCI cfg num = do
  tok <- T.readFile (circleTokenFile cfg)
  flip runCircleCI (AccountAPIToken tok) $
    getBuild (projectPoint cfg) num

getArtifactsCircleCI :: Config -> BuildNumber -> IO (Either ServantError [BuildArtifact])
getArtifactsCircleCI cfg num = do
  tok <- T.readFile (circleTokenFile cfg)
  flip runCircleCI (AccountAPIToken tok) $
    getBuildArtifacts (projectPoint cfg) num
