module DevServer.Client where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever, forM_, when)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import DevServer.Logger
import Network.WebSockets
import System.FSNotify
import System.Process.Typed

endsWith :: String -> String -> Bool
endsWith pattern str = pattern == drop (length str - length pattern) str

dropLastCharacter :: String -> String
dropLastCharacter s = take l s where l = length s - 1

watchClient :: FilePath -> FilePath -> [String] -> Int -> FastLogger -> IO ()
watchClient clientBuildDir clientSrcDir clientFileExtensions webSocketPort log = do
  fileChangedRef :: IORef Bool <- newIORef False
  browser :: MVar Connection   <- newEmptyMVar

  let buildClient = do
        log "rebuilding client"
        writeIORef fileChangedRef False
        runProcess $ setWorkingDir clientBuildDir $ shell "npm run build"

  let notifyBrowser = do
        log "notifying connection"
        conn <- tryTakeMVar browser
        case conn of
          Just c -> do
            log "sending RELOAD instruction"
            sendTextData @Text c "RELOAD"
          Nothing -> log "no browser connection currently"

  log "building client"
  _ <- buildClient
  log "watching for changes to client"

  -- start websocket server to notify client that it needs to reload
  forkIO $ do
    runServer "localhost" webSocketPort $ \pending -> do
      conn <- acceptRequest pending
      log "CONNECTION ACCEPTED"
      tryTakeMVar browser
      putMVar browser conn
      withPingThread conn 30 (pure ()) $ forever $ do
        msg <- receive conn
        sendTextData @Text conn "NOP"

  let isTriggerFileType path =
        let f ext = endsWith ("." <> ext) path
        in or (fmap f clientFileExtensions)

  -- watch for changes
  forkIO $ withManager $ \mgr -> do
    let filter = \case
          Added path _ _ -> isTriggerFileType path
          Removed path _ _ -> isTriggerFileType path
          Unknown {} -> False
          Modified path _ _ -> isTriggerFileType path
        action _ = writeIORef fileChangedRef True
    watchTree mgr clientSrcDir filter action
    forever $ threadDelay 1000000

  -- rebuild / restart server on changes
  forever $ do
    fileChanged <- readIORef fileChangedRef
    when fileChanged $ do
      log ">>> a file change has been detected <<<"
      ec <- buildClient
      when (ec == ExitSuccess) notifyBrowser
    threadDelay 2000000

