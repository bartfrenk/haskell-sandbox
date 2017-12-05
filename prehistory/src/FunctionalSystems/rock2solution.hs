module Main where
import Network
import Network.Socket hiding (accept, sClose)
import System.IO
import Control.Exception

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

parseMove :: String -> Maybe Move
parseMove str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` (" \r\n" :: String))

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h

computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  hPutStrLn h $ "You " ++ show o

withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn = do
  s <- listenOn listenPort
  (h, host, port) <- accept s
  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
  sClose s  -- Only accept one client
  a <- fn h
  hClose h
  return a

withClient' :: PortID -> (Handle -> IO a) -> IO a
withClient' listenPort fn =
  bracket (listenOn listenPort) sClose $ \s ->
    bracket (accept s) (\(h, _, _) -> hClose h) $
      \(h, host, port) -> do
        putStrLn $ "Connection from host " ++ host
                   ++ " port " ++ show port
        fn h

main :: IO ()
main = withClient' (PortNumber 1617) (computerVsUser Rock)


webServerAddr :: String -> IO [AddrInfo]
webServerAddr name = do
  addrs <- getAddrInfo Nothing (Just name) (Just "www")
  return addrs
