module ServicePattern.Logger where

import Prelude hiding (log)

import qualified Data.Text as T

data Priority
  = Debug
  | Info
  | Warning
  | Error

newtype Handle = Handle
  { log :: Priority -> T.Text -> IO () }

logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
