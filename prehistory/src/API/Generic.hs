{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module API.Generic where

import           Servant

data Schema = Schema

class HasSchema a where
  schema :: Proxy a -> Schema

class DynamicInputType d where
  data Value d
  data Param d
  data Ctx d
  resolve :: d -> Param d -> Ctx d -> IO (Value d)


type DynamicInputTypeConstraint d
  = (DynamicInputType d,
     HasSchema (Value d),
     HasSchema (Param d))

type DynamicInputTypeAPI d =
  -- "param-schema" :> Get '[JSON] Schema :<|>
  -- "value-schema" :> Get '[JSON] Schema :<|>
  "value" :> Capture "param" (Param d)
          :> QueryParam "ctx" (Ctx d)
          :> Get '[JSON] (Value d)

api :: DynamicInputTypeConstraint d => Proxy (DynamicInputTypeAPI d)
api = Proxy

