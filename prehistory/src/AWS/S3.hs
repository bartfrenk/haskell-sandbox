{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AWS.S3 where

import BasicPrelude
import Control.Lens
import Network.AWS
import Network.AWS.S3
import Network.AWS.STS
import System.IO (stdout)

putExample :: IO PutObjectResponse
putExample = do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    lgr  <- newLogger Debug stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The Credentials parameter is used to specify
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case Discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    env  <- newEnv Ireland Discover

    -- The payload (and hash) for the S3 object is retrieved from a FilePath:
    body <- readFile "local/path/to/object-payload"

    -- We now run the AWS computation with the overriden logger, performing the
    -- PutObject request. envRegion or within can be used to set the
    -- remote AWS Region:
    runResourceT $ runAWS (env & envLogger .~ lgr) $
        within Ireland $
            send (putObject "bucket-name" "object-key" (toBody body))


listBucketsExample :: IO ListBucketsResponse
listBucketsExample = do
  lgr <- newLogger Debug stdout

  env <- newEnv Ireland (FromSession
                           (AccessKey "ASIAIJTZGLBE7JIH277A")
                           (SecretKey "bJfurPt46fhPhbTyPpbTjFsXrVRf0LI9uNvlejuH")
                           (SessionToken "FQoDYXdzEGEaDF7V1gPLIh49HwUXGiKsAVV7q2E7FHftRx5s5bfPeNsOxOQDU6vAzz6yX1xh6+eDPjZGybrIEGpkHwuwg0GvFzWsruo+v+qxR5e6n3cwT8WBvHewyxd5H1IVBRNPqOdgE66qsJ9ibiOJgA3uZ6e9xxkHjMX3B4XxN9Vtl9bYbyUGpaX3xE7dKZfqYY8+7UJpBI/1yJRJa7y8zwb6WcbdlgemzEH9n5bkJzjlONh78Bd//yaywTdZ1K96svEo05WLxgU="))

  runResourceT $ runAWS (env & envLogger .~ lgr) $
    within Ireland $
    send listBuckets



assumeRoleTest :: IO AssumeRoleResponse
assumeRoleTest = do
  lgr <- newLogger Debug stdout

  env <- newEnv Ireland (FromProfile "default")

  let ar = assumeRole "arn:aws:iam::633143897132:role/AWS-Admin" "test"

  runResourceT $ runAWS (env & envLogger .~ lgr) $
    within Ireland $
    send $ (ar & arTokenCode .~ Just "680689")

