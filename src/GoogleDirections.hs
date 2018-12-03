{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module GoogleDirections where

import Data.Aeson
import GHC.Generics
import Https
import Network.HTTP.Client

newtype GoogleDirectionsResponse = GoogleDirectionsResponse {
  routes :: [Route]
} deriving (Generic, Show, FromJSON)

newtype Route = Route {
  legs :: [Leg]
} deriving (Generic, Show, FromJSON)

data Leg = Leg {
  start_address :: String,
  end_address :: String,
  distance :: TextValue,
  duration :: TextValue
} deriving (Generic, Show, FromJSON)

data TextValue = TextValue {
  text :: String,
  value :: Integer
} deriving (Generic, Show, FromJSON)

type ApiKey = String

data Mode = Driving | Bicycling | Transit

instance Show Mode where
  show Driving = "driving"
  show Bicycling = "bicycling"
  show Transit = "transit"

getGoogleDirections :: Manager -> ApiKey -> Mode -> String -> String -> Int -> IO (Maybe GoogleDirectionsResponse)
getGoogleDirections manager key mode origin destination departureTime = do
  resp <- httpsGet manager (googleDirectionsUrl mode origin destination departureTime key)
  return $ decode resp
  where
    googleDirectionsUrl :: Mode -> String -> String -> Int -> String -> ApiKey
    googleDirectionsUrl mode origin destination departureTime key = urlBase ++ "?mode=" ++ show mode ++ "&origin=" ++ origin ++ "&destination=" ++ destination ++ "&departure_time=" ++ show departureTime ++ "&key=" ++ key
    urlBase = "https://maps.googleapis.com/maps/api/directions/json"
