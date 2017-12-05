{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module API.GeoLocationAPI where

import           Servant

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             hiding (Value)
import           Data.Text
import           GHC.Generics

import           API.Generic

data GeoLocation = GeoLocation

data Country = Country
  { _countryCode  :: Text
  , _countryLabel :: Text
  } deriving (Eq, Show, Generic)


data Region = Region
  { _regionCode  :: Text
  , _regionLabel :: Text
  } deriving (Eq, Show, Generic)

data City = City
  { _cityCode  :: Text
  , _cityLabel :: Text
  } deriving (Eq, Show, Generic)

data IpAddress = IpAddress

data Location = Location
  { _country :: Country
  , _region  :: Region
  , _city    :: City
  } deriving (Eq, Show, Generic)

instance ToJSON City
instance ToJSON Region
instance ToJSON Country
instance ToJSON Location

instance ToJSON (Value GeoLocation) where
  toJSON (MkValue location) = toJSON location

instance HasSchema (Value GeoLocation) where
  schema _ = Schema

instance HasSchema (Param GeoLocation) where
  schema _ = Schema

instance FromHttpApiData (Param GeoLocation) where
  parseUrlPiece = undefined

instance FromHttpApiData (Ctx GeoLocation) where
  parseQueryParam = undefined

instance DynamicInputType GeoLocation where
  data Value GeoLocation = MkValue Location
  data Ctx GeoLocation = MkCtx IpAddress
  data Param GeoLocation = MkParam ()

  resolve _ _ _ = return $ MkValue $ Location
    (Country "NL" "Netherlands")
    (Region "NL-NB" "Noord-Brabant")
    (City "NL-EHV" "Eindhoven")

server :: DynamicInputTypeConstraint d
       => d -> Server (DynamicInputTypeAPI d)
server dyn param ctx' = case ctx' of
  Just ctx -> liftIO $ resolve dyn param ctx
  Nothing  -> undefined

app :: Application
app = serve api (server GeoLocation)
