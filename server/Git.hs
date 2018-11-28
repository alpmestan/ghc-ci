{-# LANGUAGE RecordWildCards #-}
module Git where

import Config
import Types

import Control.Monad.Except
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

{- !!!

   - The Config's private key must be accessible by the user running this
     service.
   - github.com must be a known host.

   !!! -}
cloneAndPush :: Config -> BuildRequest -> IO (Maybe GitError)
cloneAndPush Config{..} BuildRequest{..} = withEnv "GIT_SSH_COMMAND" sshCmd $ do
  r <- withTempDirectory reposDir "ghc-gitlab" $ \tmp -> runExceptT $ do
    let dir = gitDir tmp
    ls reposDir
    git ["clone", "--depth", "1", cloneUrl, tmp]
    ls reposDir
    ls tmp
    git [dir, "remote", "add", "gh", ghUrl]
    ls reposDir
    ls tmp
    git [dir, "fetch", "origin", commit source]
    ls reposDir
    ls tmp
    git [dir, "checkout", "-f", "-b", branch, commit source]
    ls reposDir
    ls tmp
    git [dir, "push", "gh", branch]

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

        sshCmd = "ssh -i " ++ githubPrivateKeyFile ++ " -F /dev/null"

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

ls :: FilePath -> ExceptT GitError IO ()
ls dir = liftIO (callCommand $ "ls -lah " ++ dir)

withEnv :: String -> String -> IO a -> IO a
withEnv key val act = do
  setEnv key val
  r <- act
  unsetEnv key
  return r
