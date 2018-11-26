{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import API
import CircleCI
import Config
import Git
import Types

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Network.CircleCI.Build
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Directory

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

data CIError
  = GitProblem GitError
  | CircleCIError ServantError

type App = ReaderT Config (ExceptT CIError IO)

server :: ServerT API App
server = newJob :<|> getJobStatus :<|> getJobArtifacts

newJob :: BuildRequest -> App BuildNumber
newJob req = do
  createGithubBranch req
  createCircleCIBuild req

createGithubBranch :: BuildRequest -> App ()
createGithubBranch req = do
  cfg <- ask
  r <- liftIO (cloneAndPush cfg req)
  maybe (pure ()) (throwError . GitProblem) r

createCircleCIBuild :: BuildRequest -> App BuildNumber
createCircleCIBuild req = do
  cfg <- ask
  r <- liftIO (buildOnCircleCI cfg req)
  case r of
    Left e -> throwError (CircleCIError e)
    Right a -> return (number a)

getJobStatus :: BuildNumber -> App BuildInfo
getJobStatus num = do
  cfg <- ask
  r <- liftIO (getStatusCircleCI cfg num)
  case r of
    Left e -> throwError (CircleCIError e)
    Right a -> return a

getJobArtifacts :: BuildNumber -> App [BuildArtifact]
getJobArtifacts num = do
  cfg <- ask
  r <- liftIO (getArtifactsCircleCI cfg num)
  case r of
    Left e -> throwError (CircleCIError e)
    Right a -> return a

checkFileExists :: FilePath -> IO ()
checkFileExists file = do
  fileExists <- doesFileExist file
  when (not fileExists) $ error $
    "Couldn't start ghc-ci-server: " ++
    "file " ++ file ++ " does not exist."

main :: IO ()
main = do
  cfg <- getConfig
  print cfg

  checkFileExists (githubPrivateKeyFile cfg)
  checkFileExists (circleTokenFile cfg)
  createDirectoryIfMissing True (reposDir cfg)

  run (httpPort cfg) . serve api $
    hoistServer api (go cfg) server

  where api = Proxy :: Proxy API
        go :: Config -> App a -> Handler a
        go cfg a = liftIO (runExceptT $ runReaderT a cfg) >>= \case
          Left e  -> throwError (toHttpErr e)
          Right x -> pure x

        toHttpErr :: CIError -> ServantErr
        toHttpErr err = err400 { errBody = encode (jsonize err) }

        jsonize (GitProblem (GitError args exit out err)) =
          object [ "error_type" .= ("git" :: String)
                 , "git_args" .= unwords args
                 , "git_exitcode" .= exit
                 , "git_stdout" .= out
                 , "git_stderr" .= err
                 ]

        jsonize (CircleCIError err) = case err of
          FailureResponse resp ->
            object [ "error_type" .= ("circleci - bad response" :: String)
                   , "circleci_response" .= strictTextOf (responseBody resp)
                   ]
          DecodeFailure e _resp ->
            object [ "error_type" .= ("circleci - failed to decode response" :: String)
                   , "decoding_error" .= e
                   ]
          UnsupportedContentType _mt _resp ->
            object [ "error_type" .= ("circleci - unsupported content type" :: String) ]
          InvalidContentTypeHeader _resp ->
            object [ "error_type" .= ("circleci - invalid content type header" :: String) ]
          ConnectionError e ->
            object [ "error_type" .= ("circleci - connection error" :: String)
                   , "error" .= e
                   ]

        strictTextOf = T.toStrict . T.decodeUtf8
