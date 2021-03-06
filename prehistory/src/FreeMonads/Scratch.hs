{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
-- | Work on the ideas presented at http://degoes.net/articles/modern-fp

module Scratch where

import           Control.Monad.Free
import           Data.List.Split
import           Prelude            hiding (log)
import Data.Functor.Sum

--------------------------------------------------------------------------------
-- The DSL for cloud files.
data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([Path] -> a)
  -- | GetFileContents Path (Bytes -> a)

type Path = String

type Bytes = String

-- | TODO: I think this should work if we write `deriving Functor`
instance Functor CloudFilesF where
  fmap f (SaveFile path bytes next) = SaveFile path bytes (f next)
  fmap f (ListFiles path withPaths) = ListFiles path (f . withPaths)
  -- fmap f (GetFileContents path withContents) =
  --   GetFileContents path (f . withContents)

saveFile :: Path -> Bytes -> Free CloudFilesF ()
-- | To define `saveFile` we use `liftF`:
--
--       liftF :: (Functor f, MonadFree f m) => f a -> m a
--
saveFile path bytes = liftF $ SaveFile path bytes ()

listFiles :: Path -> Free CloudFilesF [Path]
listFiles path = liftF $ ListFiles path id

--------------------------------------------------------------------------------
-- The DSL for logging.
data Level = Debug | Info | Warning | Error deriving Show
data LogF a = Log Level String a deriving Functor

log :: Level -> String -> Free LogF ()
log level msg = liftF $ Log level msg ()

-- | An interpreter for the logging DSL.
interpretLog :: Free LogF a -> IO ()
interpretLog (Free (Log level msg next)) = do
  putStrLn $ (show level) ++ ": " ++ msg
  interpretLog next
interpretLog (Pure _) = do
  putStrLn $ "End of program"

--------------------------------------------------------------------------------
-- The DSL for REST clients.
data RestF a = Get Path (Bytes -> a)
             | Put Path Bytes (Bytes -> a) -- | TODO: why does get returns Bytes?
             deriving Functor

get :: Path -> Free RestF Bytes
get path = liftF $ Get path id

put :: Path -> Bytes -> Free RestF Bytes
put path bytes = liftF $ Put path bytes id

-- | An interpreter for the cloud DSL that uses the REST DLS.
interpretCloudWithRest :: CloudFilesF a -> Free RestF a
interpretCloudWithRest (SaveFile path bytes next) = do
  put path bytes
  return next
-- | For this case let's do something slightly more interesting.
interpretCloudWithRest (ListFiles path withFiles) = do
  content <- get path
  let files = splitOn " " content
  return (withFiles files)

-- | An interpreter for the cloud DSL in terms of logging.
interpretCloudWithLogging :: CloudFilesF a -> Free LogF a
interpretCloudWithLogging (SaveFile path bytes next) = do
  let msg = "Saving " ++ bytes ++ " to " ++ path
  log Debug msg
  return next

interpretCloudWithLogging (ListFiles path withFiles) = do
  let msg = "Listing " ++ path
  log Debug msg
  return (withFiles []) -- FIXME: this is not nice!


-- | An interpreter for the REST DSL.

interpretRest :: Free RestF a -> IO ()
-- | Note that `withResponse :: Bytes -> Free RestF a` since
-- `Get path withResponse :: RestF (Free RestF a)`.
interpretRest (Free (Get path withResponse)) = do
  putStrLn $ "I should GET " ++ path
  result <- return "mocked GET response"
  interpretRest (withResponse result)
interpretRest (Free (Put path bytes withResponse)) = do
  putStrLn $ "I should PUT " ++ path ++ " " ++ bytes
  result <- return "mocked PUT response"
  interpretRest (withResponse result)
interpretRest (Pure x) =
  putStrLn "Finishing and ignoring the result"

-- Test the interpreter for the REST DSL with a program.
sampleProgram :: Free RestF Bytes
sampleProgram = do
  put "/artist/0" "juan"
  get "/artist/0"

runSampleProgram :: IO ()
runSampleProgram = interpretRest sampleProgram

-- Test the intepreter for the cloud DSL that used the REST DSL.
sampleCloudFilesProgram :: Free CloudFilesF ()
sampleCloudFilesProgram = do
  saveFile "/myfolder/pepino" "verde"
  saveFile "/myfolder/tomate" "rojo"
  _ <- listFiles "/myfolder"
  return ()

-- | Run the sample program using the REST interpreter.
runSampleCloudProgram :: IO ()
runSampleCloudProgram =
  interpretRest $ foldFree interpretCloudWithRest sampleCloudFilesProgram

-- | Run the sample program using the loging interpreter.
runSampleCloudProgram1 :: IO ()
runSampleCloudProgram1 =
  interpretLog $ foldFree interpretCloudWithLogging sampleCloudFilesProgram

-- infix and bind to the left maybe:
-- That way we can write
--   interpretCloudWithRest >*
--   interpretCloudWithLogging >*
--   interpretCloudWithPerformance
(>*) :: (Functor g, Functor h)
     => (forall a. f a -> g a) -> (f d -> h c) -> f d -> Free (Sum g h) d
(>*) nat s program = do
  val <- liftF (InL (nat program))
  _ <- liftF (InR (s program))
  return val

interpretCloudInSum = foldFree (interpretCloudWithRest >* interpretCloudWithLogging)

programInSum = interpretCloudInSum sampleCloudFilesProgram

interpretCloudInIO = collapse interpretRest interpretLog programInSum

collapse :: (Functor g, Functor h, Monad m)
         => (forall a. g a -> m a) -> (forall a. h a -> m a) -> Free (Sum g h) b -> m b
collapse r s (Pure z) = return z
collapse r s (Free (InR x) next) = undefined
