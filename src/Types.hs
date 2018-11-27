{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad
import Data.Aeson
import Data.Int
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Network.CircleCI.Build
import Web.HttpApiData

import qualified Data.Text as T

-- | @validate-x86_64-linux@, etc
type CircleCIJob = Text

data JobType = CircleCI CircleCIJob
  deriving Eq

instance Show JobType where
  show (CircleCI j) = "circleci-" ++ T.unpack j

instance FromJSON JobType where
  parseJSON (String s)
    | "circleci-" `T.isPrefixOf` s = pure $ CircleCI (T.drop 9 s)
    | otherwise = mzero
  parseJSON _ = mzero

instance ToJSON JobType where
  toJSON (CircleCI j) = String ("circleci-" <> j)

data Source = Source
  { user    :: String
  , project :: String
  , commit  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON Source
instance ToJSON Source

data BuildRequest = BuildRequest
  { jobType    :: JobType
  , source     :: Source
  , pipelineID :: Int64
  , runnerID   :: Int64
  , jobID      :: Int64
  } deriving (Eq, Show, Generic)

instance FromJSON BuildRequest
instance ToJSON BuildRequest

newtype BuildNum = BuildNum Int64
  deriving (Eq, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

data BuildStatus = BuildStatus String
 deriving (Eq, Show, Generic)

instance FromJSON BuildStatus
instance ToJSON BuildStatus

data BuildDetails = BuildDetails
  { build_num :: BuildNumber
  , url       :: String
  } deriving (Eq, Show, Generic)

instance FromJSON BuildDetails
instance ToJSON BuildDetails
