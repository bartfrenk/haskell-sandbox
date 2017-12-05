{-# LANGUAGE FlexibleInstances #-}

module PayloadPhantom
    (
      Part(..)
    , Param
    , ContentType

    -- We do not export any of the constructors for Payload. This
    -- keeps the type "abstract", as external users cannot directly
    -- construct values of this type.
    , Payload

    -- These are "smart constructors". We use them to provide an API
    -- that allows controlled, constrained construction of Payload
    -- values.
    , param
    , filePart
    , fileString
    ) where

import Data.Monoid

type Name = String

data Part = Part {
      name        :: Name
    , fileName    :: Maybe FilePath
    , contentType :: Maybe ContentType
    , body        :: String
    } deriving (Show)

type Param = (String, String)

type ContentType = String

data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)

param :: Name -> String -> Payload [Param]
param name value = Params [(name, value)]

filePart :: Name -> FilePath -> IO (Payload [Part])
filePart name path = do
  body <- readFile name
  return (fileString name (Just path) body)

fileString :: Name -> Maybe FilePath -> String -> (Payload [Part])
fileString name path body =
  FormData [Part name path Nothing body]

addParams = undefined

instance Monoid (Payload [Param]) where
  mempty = NoPayload
  mappend = addParams
