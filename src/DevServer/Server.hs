module DevServer.Server (
  watchServer
) where

import Cabal.Project
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.IORef
import Control.Monad (forever, when)
import Control.Concurrent (forkIO, threadDelay)
import DevServer.Logger
import System.FSNotify
import System.Process.Typed

endsWith :: String -> String -> Bool
endsWith pattern str = pattern == drop (length str - length pattern) str

isHaskellFile = endsWith ".hs"
isCabalFile = endsWith ".cabal"
isTriggerFileType path = isHaskellFile path || isCabalFile path

dropLastCharacter :: String -> String
dropLastCharacter s = take l s where l = length s - 1

-- ask cabal for the full path to the api executable
getExePath :: String -> IO String
getExePath serverExeName = do
   raw <- readProcessStdout_ $ shell $ "cabal exec which " <> serverExeName
   return $ dropLastCharacter $ toString raw

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess _           = False

watchServer :: String -> FastLogger -> IO ()
watchServer serverExeName log = do
  log "watching for changes to server"

  exePath <- getExePath serverExeName
  log $ "the server executable path is: " <> toLogStr exePath

  fileChangedRef <- newIORef False
  serverProcessRef <- newIORef Nothing

  Project {..} <- readProject "./cabal.project"
  let cabalFiles = fst <$> prjPackages

  let buildAndStartApi = do
        writeIORef fileChangedRef False
        ec <- runProcess $ proc "cabal" (["build", "--enable-tests"] <> cabalFiles)
        when (isSuccess ec) $ do
          p <- startProcess $ proc exePath []
          writeIORef serverProcessRef (Just p)

  -- watch for changes
  forkIO $ withManager $ \mgr -> do
    let filter = \case
          Added path _ _ -> isTriggerFileType path
          Removed path _ _ -> isTriggerFileType path
          Modified path _ _ -> isTriggerFileType path
          _ -> False
        action _ = writeIORef fileChangedRef True
    watchTree mgr "." filter action
    forever $ threadDelay 1000000

  buildAndStartApi

  -- rebuild / restart server on changes
  forever $ do
    fileChanged <- readIORef fileChangedRef
    when fileChanged $ do
      log ">>> a file change has been detected <<<"
      serverProcess <- readIORef serverProcessRef
      case serverProcess of
        Nothing -> return ()
        Just p -> do
          stopProcess p
          waitExitCode p
          writeIORef serverProcessRef Nothing
      buildAndStartApi
    threadDelay 2000000

