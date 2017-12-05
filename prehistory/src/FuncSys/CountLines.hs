module FuncSys.CountLines (recDir) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath
import System.Posix
import System.IO
import System.IO.Unsafe
import Criterion.Main

readFiles :: [FilePath] -> IO L.ByteString
readFiles [] = pure L.empty
readFiles (f:fs) = liftM2 L.append (L.readFile f)
                   (unsafeInterleaveIO $ readFiles fs)

readFiles' :: [FilePath] -> IO L.ByteString
readFiles' fs = foldM op L.empty fs
  where op :: L.ByteString -> FilePath -> IO L.ByteString
        op bs f = (`L.append` bs) `fmap` L.readFile f


countLines :: FilePath -> IO ()
countLines dir =
  listDir dir >>= readFiles >>= print . L8.count '\n'

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
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat =
              liftM2 (++) (protect $ recDir path) nextName
          | otherwise = nextName

  nextName





listDir :: FilePath -> IO [FilePath]
listDir dir =
  bracket (openDirStream dir) closeDirStream (listDirStream dir)

listDirStream :: FilePath -> DirStream -> IO [FilePath]
listDirStream dir ds =
  readDirStream ds >>= nextName
  where nextName ""   = return []
        nextName "."  = listDirStream dir ds
        nextName ".." = listDirStream dir ds
        nextName name = getSymbolicLinkStatus path >>= recurse path
          where path = dir </> name
        recurse path stat
          | isRegularFile stat = liftM (path :) (listDirStream dir ds)
          | isDirectory stat = liftM2 (++) (listDir path) (listDirStream dir ds)
          | otherwise = listDirStream dir ds

