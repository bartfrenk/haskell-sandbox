{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module AWS.TurbineStore
  (listTurbineUpdates,
   fetchTurbineUpdate,
   markTurbineUpdateDone,
   TurbineUpdate
  ) where

import           BasicPrelude         hiding (try)
import           Control.Monad.Logger
import           Data.Time.Clock
import           Data.UUID
import           Network.AWS
import           Network.AWS.S3
import           Network.AWS.STS      hiding (Credentials)
import           Text.Parsec


type Prefix = Text

data TurbineUpdateType
  = SegmentDefinitions | SegmentMemberships deriving (Eq, Show)

data TurbineUpdate = TurbineUpdate
  { updateType :: TurbineUpdateType
  , market     :: Text
  , date       :: UTCTime
  , prefix     :: Prefix
  , identifier :: UUID
  } deriving (Eq, Show)

instance Ord TurbineUpdate where
  compare s t = compare (date s) (date t)

objectKey :: TurbineUpdate -> ObjectKey
objectKey = undefined

turbineUpdate :: ObjectKey -> Either ParseError TurbineUpdate
turbineUpdate (ObjectKey s) = parse turbineUpdateParser "" s

turbineUpdateParser :: Parsec Text u TurbineUpdate
turbineUpdateParser = undefined

listTurbineUpdates :: (MonadIO m, MonadLogger m)
  => Credentials -> BucketName -> Prefix -> m [TurbineUpdate]
listTurbineUpdates = undefined

fetchTurbineUpdate :: (MonadIO m, MonadLogger m)
  => Credentials -> BucketName -> TurbineUpdate -> m FilePath
fetchTurbineUpdate = undefined

markTurbineUpdateDone :: (MonadIO m, MonadLogger m)
  => Credentials -> BucketName -> TurbineUpdate -> m ()
markTurbineUpdateDone = undefined

