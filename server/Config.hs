module Config where

import Data.Monoid
import Options.Applicative

data Config = Config
  { githubUser :: String
  , githubProject :: String
  , circleTokenFile :: FilePath
  , reposDir :: FilePath
  , gitlabHost :: String
  , httpPort :: Int
  } deriving (Eq, Show)

cfgParser :: Parser Config
cfgParser = Config
        <$> strOption
            ( long "user"
           <> short 'u'
           <> metavar "GITHUB_USERNAME"
           <> help "Username for the staging github repository"
            )
        <*> strOption
            ( long "project"
           <> short 'p'
           <> metavar "GITHUB_PROJECT_NAME"
           <> help "Project name for the staging github repository"
            )
        <*> strOption
            ( long "circleci"
           <> short 'c'
           <> metavar "CIRCLECI_TOKEN_PATH"
           <> help "Path to a file that just contains the Circle CI token"
            )
        <*> strOption
            ( long "repos"
           <> short 'r'
           <> metavar "PATH"
           <> help "Path to the directory in which the repositories will be cloned"
            )
        <*> strOption
            ( long "gitlab"
           <> short 'g'
           <> metavar "HOST"
           <> help "Host of the gitlab instance"
            )
        <*> option auto
            ( long "port"
           <> metavar "PORT"
           <> help "Port on which the HTTP server accepts build requests"
            )

getConfig :: IO Config
getConfig = execParser opts

  where opts = info (cfgParser <**> helper)
                    ( fullDesc
                   <> progDesc "GHC CI server"
                   <> header "ghc-ci - CI server for GHC's Gitlab"
                    )
