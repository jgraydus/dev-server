{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
module DevServer where

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
  , webSocketPort :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

runDevServer :: DevServerConfig -> IO ()
runDevServer DevServerConfig {..} = withLogger $ \log -> do

  concurrently 
    (watchServer serverExeName (changeColor Green log))
    (watchClient clientBuildDir clientSrcDir webSocketPort (changeColor Red log))

  log "DONE"

