module DevServer.Logger (
  changeColor,
  Color(..),
  FastLogger,
  LogStr,
  toLogStr,
  withLogger,
) where

import System.Log.FastLogger

data Color = Green | Red | Reset

-- character sequence to change terminal text color
-- format: \033[38;2;${red};${green};${blue}m
-- color values are 0-255
asLogStr :: Color -> LogStr
asLogStr Green = toLogStr ("\x1b[38;2;78;154;6m" :: String)
asLogStr Red   = toLogStr ("\x1b[38;2;154;6;6m" :: String)
asLogStr Reset = toLogStr ("\x1b[0m" :: String)

changeColor :: Color -> FastLogger -> FastLogger
changeColor color logger logStr = logger (asLogStr color <> logStr <> asLogStr Reset)

withLogger :: (FastLogger -> IO a) -> IO a
withLogger io = do
  loggerSet <- newStdoutLoggerSet defaultBufSize
  let logger = pushLogStrLn loggerSet
  io logger

