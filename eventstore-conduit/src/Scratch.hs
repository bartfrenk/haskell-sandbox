{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Scratch where

import           BasicPrelude
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Conduit                hiding (connect)
import qualified Data.Conduit.List           as CL
import           Database.EventStore         hiding (subscribe)

import           Database.EventStore.Conduit
import           Database.EventStore.Event

defaultContext :: MonadIO m => m Context
defaultContext =
  let settings = defaultSettings { s_credentials = Just creds }
  in mkContext settings (Static "127.0.0.1" 1113)

myClusterSettings :: ClusterSettings
myClusterSettings = undefined

parseEvents :: (Monad m, FromEvent a) => Conduit ResolvedEvent m a
parseEvents = CL.map fromEvent

creds :: Credentials
creds = credentials "admin" "changeit"

printer :: (MonadIO m, Show s) => Sink s m ()
printer = CL.mapM_ (liftIO . print)

act :: EventStoreMonad m => m ()
act = runConduit (fromStreamBetween "$streams" 1000 1100 $$ printer)

test :: IO ()
test = do
  context <- defaultContext
  runReaderT act context

createEventJSON :: ToJSON a => a -> Event
createEventJSON i = createEvent "test" Nothing (withJson i)

putList :: (ToJSON a, EventStoreMonad m) => StreamName -> [a] -> m ()
putList stream xs =
  let conduit = CL.sourceList xs .| CL.map createEventJSON .| toStream stream
  in runConduit conduit

testPut :: ToJSON a => [a] -> IO ()
testPut xs = do
  context <- defaultContext
  runReaderT (putList "test" xs) context

getJSON :: FromJSON a => ResolvedEvent -> Maybe a
getJSON event =
  recordedEventData <$> resolvedEventRecord event >>= decodeStrict

mappendStream :: (FromJSON a, Monoid a, EventStoreMonad m)
              => StreamName -> Int32 -> Int32 -> m a
mappendStream stream from to =
  let conduit =
        fromStreamBetween stream from to .|
        CL.mapMaybe getJSON .|
        CL.foldMap (\x -> x)
  in runConduit conduit

newtype Sum a = Sum { getSum :: a } deriving (Eq, Show)

newtype Prod a = Prod { getProd :: a } deriving (Eq, Show)

instance FromJSON a => FromJSON (Sum a) where
  parseJSON v = Sum <$> parseJSON v

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  (Sum x) `mappend` (Sum y) = Sum $ x + y

instance FromJSON a => FromJSON (Prod a) where
  parseJSON v = Prod <$> parseJSON v

instance Num a => Monoid (Prod a) where
  mempty = Prod 1
  (Prod x) `mappend` (Prod y) = Prod $ x * y

testMonoid :: (FromJSON a, Monoid a) => StreamName -> Int32 -> Int32 -> IO a
testMonoid stream from to = do
  context <- defaultContext
  runReaderT (mappendStream stream from to) context

