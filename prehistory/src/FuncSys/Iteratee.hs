module FuncSys.Iteratee where


import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.FilePath
import           System.IO
import           System.IO.Unsafe           (unsafeInterleaveIO)
import           System.Posix


recDir :: FilePath -> IO [FilePath]
recDir dir = do
  ds <- openDirStream dir
  let protect m = m `onException` closeDirStream ds

      nextName = unsafeInterleaveIO $
                 protect (readDirStream ds) >>= checkName

      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat = fmap (path :) nextName
          | isDirectory stat =
              liftM2 (++) (protect $ recDir path) nextName
          | otherwise = nextName

  nextName



data Chunk = Chunk { chunkData  :: !L.ByteString
                   , chunkAtEOF :: !Bool } deriving (Show)

newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

instance Show a => Show (Result a) where
  show (Done result residual) = "Done " ++ show result ++ " " ++ show residual
  show (NeedInput _)          = "NeedInput" -- ++ show (runIter iter chunkEOF)
  show (NeedIO _)             = "NeedIO"
  show (Failed exc)           = "Failed " ++ show exc

readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
  where go acc (Chunk input eof)
          | not (L.null b) = Done (Just acca) (Chunk btail eof)
          | not eof        = NeedInput (Iter (go acca))
          | otherwise      = Done Nothing (Chunk acca eof)
          where (a, b) = L8.break (== '\n') input
                acca = L.append acc a
                btail = L.tail b

type Enumerator a = Iter a -> IO (Result a)

type Inum a = Iter a -> Iter (Result a)



-- DEBUG: Show instance needed for tracing
enumerateFile :: FilePath -> Enumerator a
enumerateFile path iter0 =
  withFile path ReadMode $ \h ->
    let go iter = do
          input <- S.hGetSome h 200
          if S.null input
            then return (NeedInput iter)
            else check $ runIter iter $
                 Chunk (L.fromChunks [input]) False
        check (NeedInput iter) = go iter
        check (NeedIO iter)    = iter >>= check
        check result           = return result
    in go iter0

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

getResult0 :: Result a -> IO a
getResult0 (Done a _)           = return a
getResult0 (NeedInput (Iter f)) = getResult0 (f chunkEOF)
getResult0 (NeedIO io)          = io >>= getResult0
getResult0 (Failed e)           = throwIO e

nlines0 :: Iter Int
nlines0 = Iter (go 0)
  where go n c0 = check (runIter readLine c0)
          where
            check (NeedInput (Iter f)) = NeedInput (Iter (check . f))
            check (Done (Just _) c)    = go (n + 1) c
            check (Done Nothing c)     = Done n c
            check (NeedIO r)           = NeedIO (fmap check r)
            check (Failed e)           = Failed e

instance Functor Iter where
  f `fmap` iter = Iter $ \c -> check (runIter iter c)
    where check (Done a c)        = Done (f a) c
          check (NeedInput iter') = NeedInput (f `fmap` iter')
          check (NeedIO io)       = NeedIO (check `fmap` io)
          check (Failed e)        = Failed e

instance Applicative Iter where
  pure = return
  (<*>) = ap

instance Monad Iter where
  return a = Iter $ Done a
  m >>= k = Iter $ \c -> check (runIter m c)
    where check (Done a c)     = runIter (k a) c
          check (NeedInput m') = NeedInput (m' >>= k)
          check (NeedIO io)    = NeedIO (fmap check io)
          check (Failed e)     = Failed e
  fail msg = iterThrow (ErrorCall msg)


iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

nlines1 :: Iter Int
nlines1 = go 0
  where go n = readLine >>= check n
        check n (Just _) = go $! n + 1
        check n Nothing  = return n

nlines2 :: Iter Int
nlines2 = go 0
  where go n = do
          res <- readLine
          case res of
            Just _  -> go $! n + 1
            Nothing -> return n

cat0 :: Enumerator a -> Enumerator a -> Enumerator a
cat0 a b iter = a iter >>= check
  where check (NeedInput iter') = b iter'
        check (NeedIO io)       = io >>= check
        check r                 = return r

-- Enumerator a = Iterator a -> IO (Result a)
enumerateNull :: Enumerator a
enumerateNull = return . NeedInput

countLines0 :: FilePath -> IO Int
countLines0 dir = do
  files <- recDir dir
  let enumerator = foldr (cat0 . enumerateFile) enumerateNull files
  enumerator nlines1 >>= getResult0

instance MonadIO Iter where
  liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
    where mkResult _ (Left e)  = return (Failed e)
          mkResult c (Right a) = return (Done a c)


-- newtype Iter a = Iter { runIter :: Chunk -> Result a }

-- data Result a = Done { rResult :: a, rResidual :: Chunk }
--               | NeedInput !(Iter a)
--               | NeedIO !(IO (Result a))
--               | Failed !SomeException
-- data Chunk = Chunk { chunkData  :: !L.ByteString
--                    , chunkAtEOF :: !Bool } deriving (Show)



iterChunk :: Iter Chunk
iterChunk = Iter $ \c@(Chunk buf eof) ->
  if L.null buf && not eof
  then NeedInput iterChunk
  else Done c (Chunk L.empty eof)

iterStdout :: Iter ()
iterStdout = do
  (Chunk buf eof) <- iterChunk
  liftIO $ L.putStr buf
  unless eof iterStdout
