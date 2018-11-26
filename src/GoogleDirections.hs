{-# LANGUAGE RankNTypes, FlexibleInstances, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, GADTs, DeriveGeneric #-}
module GoogleDirections where

import Data.Aeson
import GHC.Generics
import Https
import Network.HTTP.Client

data GoogleDirectionsResponse = GoogleDirectionsResponse {
  routes :: [Route]
} deriving (Generic, Show)

instance FromJSON GoogleDirectionsResponse

data Route = Route {
  legs :: [Leg]
} deriving (Generic, Show)

data Leg = Leg {
  start_address :: String,
  end_address :: String,
  distance :: TextValue,
  duration :: TextValue
} deriving (Generic, Show)

data TextValue = TextValue {
  text :: String,
  value :: Integer
} deriving (Generic, Show)

instance FromJSON Leg
instance FromJSON Route
instance FromJSON TextValue

data Mode = Driving | Bicycling | Transit

instance Show Mode where
  show Driving = "driving"
  show Bicycling = "bicycling"
  show Transit = "transit"

getGoogleDirections :: Manager -> String -> Mode -> String -> String -> Int -> IO (Maybe GoogleDirectionsResponse)
getGoogleDirections manager key mode origin destination departureTime = do
  resp <- httpsGet manager (googleDirectionsUrl mode origin destination departureTime key)
  return $ decode resp
  where
    googleDirectionsUrl :: Mode -> String -> String -> Int -> String -> String
    googleDirectionsUrl mode origin destination departureTime key = urlBase ++ "?mode=" ++ show mode ++ "&origin=" ++ origin ++ "&destination=" ++ destination ++ "&departure_time=" ++ show departureTime ++ "&key=" ++ key
    urlBase = "https://maps.googleapis.com/maps/api/directions/json"
