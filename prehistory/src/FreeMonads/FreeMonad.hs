{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module FreeMonad where

import Prelude hiding (log)

import Data.List.Split
import Control.Applicative (Const(..))
import Control.Comonad
import Control.Monad.Free
import Data.Functor.Sum (Sum(..))


type Bytes = String
type Path = String

data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([Path] -> a) deriving Functor

instance Show (CloudFilesF a) where
  show (SaveFile p _ _) = "SaveFile " ++ show p
  show (ListFiles p _) = "ListFiles " ++ show p

type CloudFilesAPI a = Free CloudFilesF a

saveFile :: Path -> Bytes -> CloudFilesAPI ()
saveFile path bytes = liftF (SaveFile path bytes ())

listFiles :: Path -> CloudFilesAPI [Path]
listFiles path = liftF (ListFiles path id)

data HttpF a
  = GET Path (Bytes -> a)
  | PUT Path Bytes (Bytes -> a)
  | POST Path Bytes (Bytes -> a)
  | DELETE Path (Bytes -> a) deriving Functor

type HttpAPI a = Free HttpF a

httpI :: CloudFilesF a -> HttpAPI a
httpI = undefined

data Level = Debug | Info | Warn | Error deriving Show

data LogF a = Log Level String a deriving (Functor, Show)

type LogAPI a = Free LogF a

log :: Level -> String -> LogAPI ()
log level msg = liftF $ Log level msg ()

logI :: CloudFilesF a -> LogAPI ()
logI act = log Debug (show act)

-- Doesn't work: logging stops after first statement
constLogI :: CloudFilesF a -> (Const (LogF ())) a
constLogI act = Const (Log Debug (show act) ())

constLog :: CloudFilesAPI a -> Free (Const (LogF ())) a
constLog = hoistFree constLogI

program :: CloudFilesAPI ()
program = do
  saveFile "a" "a"
  saveFile "b" "b"
  _ <- listFiles "c"
  saveFile "d" "e"

type CombinedF = Sum HttpF LogF

type CombinedAPI a = Free CombinedF a

get :: Path -> Free HttpF Bytes
get path = liftF $ GET path id

put :: Path -> Bytes -> Free HttpF Bytes
put path bytes = liftF $ PUT path bytes id

-- | An interpreter for the cloud DSL that uses the REST DLS.
interpretCloudWithRest :: CloudFilesF a -> Free HttpF a
interpretCloudWithRest (SaveFile path bytes next) = do
  put path bytes
  return next
-- | For this case let's do something slightly more interesting.
interpretCloudWithRest (ListFiles path withFiles) = do
  content <- get path
  let files = splitOn " " content
  return (withFiles files)

combinedI :: CloudFilesF a -> CombinedAPI a
combinedI act@(SaveFile path bytes next) = do
  liftF (InL (PUT path bytes id))
  liftF (InR (Log Debug (show act) ()))
  return next

combinedI act@(ListFiles path withFiles) = do
  content <- liftF (InL (GET path id))
  InR <$> (logI act)
--  liftF (InR (Log Debug (show act) content))
  let files = splitOn " " content
  return (withFiles files)

combinedInterpreter :: CloudFilesAPI a -> CombinedAPI a
combinedInterpreter = foldFree combinedI



-- | Free monad of the Hom functor

data Hom a b = Hom (a -> b)

instance Functor (Hom a) where
   fmap f (Hom g) = Hom (f . g)

type HomFree a b = Free (Hom a) b

