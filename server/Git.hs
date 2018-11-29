{-# LANGUAGE RecordWildCards #-}
module Git where

import Config
import Types

import Control.Monad.Except
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

{- !!!

   - github.com must be a known host.
   - the user running this service must have an ssh key that
     allows it to push to the github repository at @ghUrl@

   !!! -}
cloneAndPush :: Config -> BuildRequest -> IO (Maybe GitError)
cloneAndPush Config{..} BuildRequest{..} = do
  r <- withTempDirectory reposDir "ghc-gitlab" $ \tmp -> runExceptT $ do
    let dir = gitDir tmp
    git ["clone", cloneUrl, tmp]
    git [dir, "push", ghUrl, commit source ++ ":refs/heads/" ++ branch]

  return $ either (Just . id) (const Nothing) r

  where gitDir tmp = "--git-dir=" ++ (tmp </> ".git")

        -- e.g gitlab/ghc/ghc/239/circleci-validate-x86_64-linux/21389-5
        branch = "gitlab" ++
          "/" ++ user source ++
          "/" ++ project source ++
          "/" ++ show pipelineID ++
          "/" ++ show jobType ++
          "/" ++ show jobID ++ "-" ++ show runnerID

        cloneUrl = gitlabHost ++ "/" ++ user source ++ "/" ++ project source
                ++ ".git"

        ghUrl = "git@github.com:" ++ githubUser ++ "/" ++ githubProject
             ++ ".git"

data GitError = GitError
  { gitArgs   :: [String]
  , gitExit   :: Int
  , gitStdout :: String
  , gitStderr :: String
  } deriving (Eq, Show)

git :: [String] -> ExceptT GitError IO ()
git args = do
  (ex, out, err) <- liftIO (readProcessWithExitCode "git" args "")
  case ex of
    ExitSuccess -> return ()
    ExitFailure n -> throwError (GitError args n out err)
