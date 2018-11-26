{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import Network.CircleCI.Build
import Network.CircleCI.Common.Types ()
import Servant.API
import Types

type API =
  "job" :> ReqBody '[JSON] BuildRequest :> Post '[JSON] BuildNumber :<|>
  "job" :> Capture "build num" BuildNumber :> Get '[JSON] BuildInfo :<|>
  "job" :> Capture "build num" BuildNumber :> "artifacts" :> Get '[JSON] [BuildArtifact]
