{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module FuncSys.Scratch where

import           Data.Aeson   (ToJSON)
import qualified Data.Aeson   as JSON
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           GHC.Generics
import           Prelude      hiding (id)

fix :: (a -> a) -> a
fix f = let x = f x in x

inc :: Int -> Int
inc = (+) 1

id :: forall a. a -> a
id x = x


test :: Num a => Integer -> a
test = fromInteger


-- class DynamicInput a v | a -> v where
--   resolve :: a -> v
--   deserialize :: String -> a

-- data GeoLocation v = GeoLocation v

-- data Country = NL deriving Show

-- instance DynamicInput (GeoLocation Country) Country where
--   resolve (GeoLocation c) = c
--   deserialize _ = GeoLocation NL


-- f :: DynamicInput a v => String -> a
-- f = deserialize

-- data LocationParams = LocationParams
--   { country :: Maybe String
--   }

-- data Location = Location
--   { country :: String
--   , region :: String
--   , city :: String
--   }

-- data ImpressionContext = ImpressionContext

-- mkLocation :: LocationParams -> DynamicInput IO Location
-- mkLocation = undefined

-- parseDynamicInput :: String ->

-- data DynamicInput m a = DynamicInput
--   { resolve :: ImpressionContext -> m a
--   , keys :: m [a]
--   }


-- class DynamicInput dyn val where
--   resolve :: dyn -> IO val

-- data TurbineSegment = TurbineSegment
--   {

--                                      }

type MarketCode = String
type CountryCode = String
type ProductSource a = [a]
type Rule = String
type ImpressionContext = String
type Schema = String

data TurbineSegment = TurbineSegment
  { market :: MarketCode
  }

data Region = Region
  { country :: CountryCode
  }

data ProductSelector a = ProductSelector
  { source :: ProductSource a
  , count  :: Int
  , rule   :: Rule
  }

-- data DynamicInput b = DynamicInput
--   { resolve :: ImpressionContext -> IO b
--   , keys :: IO [b]
--   }

data Location = Location
  { city    :: String
  , country :: String
  , region  :: String
  } deriving (Generic, Show)

instance ToJSON Location

class HasSchema m b where
  schema :: b -> m Schema

instance HasSchema IO Location where
  schema _ = return "schema"

data Resolver m b = Resolver { resolve :: ImpressionContext -> m b }
  deriving Functor

instance (Monoid a, Monad m) => Monoid (Resolver m a) where
  mempty = Resolver $ \_ -> return mempty
  mappend (Resolver f) (Resolver g) = Resolver $ \ctx ->
    mappend <$> f ctx <*> g ctx

class Resolvable m a where
--  type Result a :: *
  resolver :: a -> Resolver m (Result a)

instance Resolvable IO Region where
  -- type Result Region = Location
  resolver _ = Resolver (\_ -> return $ Location "Eindhoven" "NL" "Brabant")

instance (Monad m, Traversable t, Resolvable m v) => Resolvable m (t v) where
  --  type Result (t v) = t (Result v)
  resolver = traverse resolver

instance Monad m => Applicative (Resolver m) where
  pure x = Resolver (\_ -> return x)
  f <*> u = Resolver $ \ctx -> resolve f ctx <*> resolve u ctx

type family Result a
type instance Result Region = Location
type instance 

getSchema :: (Monad m, Resolvable m a, HasSchema m (Result a)) => a -> m Schema
getSchema res = schema res

-- resultProxy :: Resolvable m a => a -> Proxy (Result a)
-- resultProxy _ = Proxy

jsonResolver :: (Monad m, Resolvable m a, ToJSON (Result a)) => a -> Resolver m JSON.Value
jsonResolver res = JSON.toJSON `fmap` resolver res
