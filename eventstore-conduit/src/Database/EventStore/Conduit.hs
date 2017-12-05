{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.EventStore.Conduit where

import           BasicPrelude
import           Control.Concurrent.Async
import           Control.Lens             hiding (Context, from, to)
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Conduit             hiding (connect)
import           Database.EventStore      hiding (subscribe)

data Context = Context
  { _connection :: Connection
  , _batchSize  :: Int32
  }

makeLenses ''Context

mkContext :: MonadIO m => Settings -> ConnectionType -> m Context
mkContext settings ty = do
  conn <- liftIO $ connect settings ty
  return $ Context conn 100

type EventStoreMonad m = (MonadReader Context m, MonadIO m, MonadThrow m)

data EventStoreException
  = GenericException
  deriving (Show, Typeable)

instance Exception EventStoreException

-- |Source that reads stream 'stream' and produces all events from event number
-- 'from' up to and excluding 'to' in order.
fromStreamBetween :: EventStoreMonad m
                  => StreamName -> Int32 -> Int32 -> Source m ResolvedEvent
fromStreamBetween stream from to = do
  conn <- view connection
  n <- view batchSize
  let m = min (to - from) n
  unless (m <= 0) $ do
    as <- liftIO $ readStreamEventsForward conn stream from m True
    -- TODO: time out when wait takes too long, timeout value in context
    result <- liftIO $ wait as
    case result of
      ReadSuccess slice -> do
        mapM_ yield $ sliceEvents slice
        fromStreamBetween stream (from + n) to
      ReadNoStream -> throwM GenericException
      ReadStreamDeleted _ -> throwM GenericException
      ReadNotModified -> throwM GenericException
      ReadError _ -> throwM GenericException
      ReadAccessDenied _ -> throwM GenericException

toStream :: EventStoreMonad m
         => StreamName -> Sink Event m ()
toStream stream = do
  conn <- view connection
  event' <- await
  case event' of
    Nothing -> return ()
    Just event -> do
      as <- liftIO $ sendEvent conn stream anyVersion event
      void (liftIO $ wait as)
      toStream stream

