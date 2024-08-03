{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
module DevServer (
    defaultDevServerConfig,
    DevServerConfig(..),
    runDevServer,
    module DevServer.Client,
    module DevServer.Server,
    module DevServer.Logger,
) where

import Control.Concurrent.Async (concurrently)
import Data.Aeson (FromJSON, ToJSON)
import DevServer.Client
import DevServer.Server
import DevServer.Logger
import GHC.Generics (Generic)

data DevServerConfig = DevServerConfig
  { serverExeName :: String
  , clientBuildDir :: FilePath
  , clientSrcDir :: FilePath
  , clientFileExtensions :: [String]
  , webSocketPort :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

defaultDevServerConfig :: DevServerConfig
defaultDevServerConfig =
  DevServerConfig
  { serverExeName = "server"
  , clientBuildDir = "./client"
  , clientSrcDir = "./client/src"
  , clientFileExtensions = ["ts", "tsx"]
  , webSocketPort = 8082
  }

runDevServer :: DevServerConfig -> IO ()
runDevServer DevServerConfig {..} = withLogger $ \log -> do

  concurrently 
    (watchServer serverExeName (changeColor Green log))
    (watchClient clientBuildDir clientSrcDir clientFileExtensions webSocketPort (changeColor Red log))

  log "DONE"

