module FuncSys.Lenses where

loop :: Bool
loop = loop


data Outliers

data OutlierEffect
  = Unaffected
  | Slight
  | Moderate
  | Severe

data OutlierVariance = OutlierVariance {
    ovEffect :: OutlierEffect
  , ovDescription :: String
  , ovFraction :: Double
  }

data SampleAnalysis = SampleAnalysis {
    anMean :: [Double]
  , anStdDev :: [Double]
  , anOutlierVar :: OutlierVariance
  }

data Payload = Payload {
    sample :: [Double]
  , sampleAnalysis :: SampleAnalysis
  , outliers :: Outliers
  }
