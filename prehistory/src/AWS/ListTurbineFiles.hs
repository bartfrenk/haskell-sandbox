{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module AWS.ListTurbineFiles where


import           BasicPrelude         hiding (try)
import           Control.Lens
import           Control.Monad.Except
import           Data.Maybe
import           Data.String.Conv
import qualified Data.Text            as Text
import           Data.Time.Clock
import           Data.Time.Format
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import           Network.AWS
import           Network.AWS.S3
import           Network.AWS.STS      hiding (Credentials)
import           System.IO
import           Text.Parsec

data TurbineObjectType
  = SegmentDefinitions | SegmentMemberships deriving (Eq, Show)

data TurbineObject = TurbineObject
  { processed  :: Bool
  , objectType :: TurbineObjectType
  , marketCode :: String
  , date       :: UTCTime
  , ident      :: UUID
  } deriving (Eq, Show)


-- turbineObjects' :: IO [Either ParseError TurbineObject]
-- turbineObjects' = do
--   keys <- turbineKey
--   return $ parseObjectKey . keyToText <$> kebs
--     where keyToText (ObjectKey txt) = txt

-- listTurbineObjects :: Prefix -> IO [TurbineObject]
-- listTurbineObjects prefix = undefined


-- turbineKey :: IO [ObjectKey]
-- turbineKey = do
--   objects <- devCreds >>= listTurbineFiles
--   return $ view oKey `fmap` (objects ^. lorsContents)

makeTime :: Monad m => String -> m UTCTime
makeTime = parseTimeM False defaultTimeLocale "%0Y%m%d%H%M%S"

makeUUID :: Monad m => String -> m UUID
makeUUID s = maybe (fail "not a UUID") return (UUID.fromString s)

data TurbineUpdate = TurbineUpdate
  { updateType :: TurbineObjectType
  , market     :: String
  , date       :: UTCTime
  , uuid       :: UUID
  } deriving (Eq, Show)

objectNameParser :: Parsec Text u TurbineUpdate
objectNameParser = do
  _    <- string "turbine-lpi-segment-"
  ty   <- choice [string "def" >> return SegmentDefinitions,
                  string "members" >> return SegmentMemberships]
  _    <- char '-'
  mc   <- manyTill anyChar (try (char '-'))
  time <- manyTill anyChar (try (char '-')) >>= makeTime
  uuid <- manyTill anyChar (try (char '.')) >>= makeUUID
  return $ TurbineUpdate ty mc time uuid

parseObjectName :: Text -> Either ParseError TurbineUpdate
parseObjectName = parse objectNameParser ""

type Prefix = Text

listObjectNames :: Credentials -> BucketName -> Prefix -> IO [Text]
listObjectNames creds bucketName prefix = do
  env <- newEnv creds
  let cmd = listObjects bucketName & loPrefix .~ Just prefix
  resp <- runResourceT $ runAWS env $
    within Ireland $ send cmd
  let names = view (oKey . keyName '/') `fmap` (resp ^. lorsContents)
  return $ filter (not . Text.null) names

test :: (MonadIO m, MonadError ParseError m) => m TurbineUpdate
test = do
  creds <- fromJust . extractCredentials <$> assumeDeveloperRole
  names <- listObjectNames creds "lemonpi-turbine-export" "inbox/turbine-lpi-segment-21"
  return $ parseObjectName <$> names

listAllBuckets :: Credentials -> IO ListBucketsResponse
listAllBuckets creds = do
  logger <- newLogger Debug stdout
  env <- newEnv creds

  runResourceT $ runAWS (env & envLogger .~ logger) $
    within Ireland $
    send listBuckets

assumeDeveloperRole :: IO AssumeRoleResponse
assumeDeveloperRole = do
  env <- newEnv (FromFile "default" "/home/bart/.aws/credentials")

  runResourceT $ runAWS env $
    within Ireland $
    send (assumeRole "arn:aws:iam::633143897132:role/Developer" "dev")

extractCredentials :: AssumeRoleResponse -> Maybe Credentials
extractCredentials resp =
  case resp ^. arrsCredentials of
    Nothing -> Nothing
    Just creds ->
      let accessKey = AccessKey $ toS (creds ^. cAccessKeyId)
          secretKey = SecretKey $ toS (creds ^. cSecretAccessKey)
          sessionToken = SessionToken $ toS (creds ^. cSessionToken)
      in Just $ FromSession accessKey secretKey sessionToken


