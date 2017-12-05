{-# LANGUAGE NoMonomorphismRestriction #-}
module AWS.Conduit where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Combinators     as C
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Prelude

main :: IO ()
main =
  print (runConduitPure $ C.enumFromTo 1 100 .| C.sum :: Int)

readFileC :: MonadResource m => FilePath -> m [Text]
readFileC path = sourceToList $
  C.sourceFile path
  .| C.decodeUtf8

parseLine :: a -> a
parseLine = id

printFileC :: MonadResource m => FilePath -> ConduitM a c m ()
printFileC path =
  C.sourceFile path
  .| C.decodeUtf8
  .| C.concatMap Text.lines
  .| C.map parseLine
  .| C.mapM_ (liftIO . print)

