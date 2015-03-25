{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import qualified Data.ByteString.Char8 as Char8

import Network.HTTP.Conduit (simpleHttp, httpLbs)
import Network.HTTP.Client (withManager, defaultManagerSettings, parseUrl,
                            requestHeaders, responseBody)
import Data.ByteString.Lazy.Search (indices)
import Data.ByteString.Lazy.Char8 (unpack, take, drop)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.Aeson (FromJSON(parseJSON), eitherDecode', (.:), Value(Object))
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Numeric (readSigned, readFloat)
import Prelude hiding (take, drop)

main :: IO ()
main = undefined

mein_wetter :: ForecastCreds -> Location -> IO String
mein_wetter anmelden wo = do
    fmap unpack . simpleHttp $ forecast_uri anmelden wo

forecast_uri anmelden wo =
    "https://api.forecast.io/forecast/" ++ anmelden ++ "/"
                                        ++ show (loc_lat wo) ++ ","
                                        ++ show (loc_lon wo)

data Forecast
    = Forecast
    { apparentTemperature :: Double
    , cloudCover :: Double
    , dewPoint :: Double
    , humidity :: Double
    , icon :: Icon
    , ozone :: Double
    , precipIntensity :: Double
    , precipProbability :: Double
    , pressure :: Double
    , summary :: String
    , temperature :: Double
    , visibility :: Double
    , windBearing :: Double
    , windSpeed :: Double
    }

data Icon = ClearDay | ClearNight | Rain | Snow | Sleet | Wind | Fog | Cloudy
          | PartlyCloudyDay | PartlyCloudyNight
    deriving Show

instance FromJSON Icon where
    parseJSON v = return $ case v of
        "clear-day" -> ClearDay
        "clear-night" -> ClearNight
        _ -> error "asdf"
    -- parseJSON _ = mzero

type ForecastCreds = String

-- | Please use with prudence.
bry_creds = "43b5e41b97e7f531b26cf82f71784b6a"

get_ipinfodb :: IO Location
get_ipinfodb = do
    html <- simpleHttp "http://ipinfodb.com/"
    return $ parse html

data Location
    = Location
    { loc_name :: String
    , loc_lat :: Double
    , loc_lon :: Double
    }
    deriving Show

parse str = Location "" lat lon
    where
    lat = read_coord $ drop (skip_to "Latitude : ") str
    lon = read_coord $ drop (skip_to "Longitude : ") str
    skip_to s = fromIntegral (head $ indices s str) + fromIntegral (Char8.length s)

read_coord bs = fst . head $ readSigned readFloat bs__
    where bs__ = unpack $ take 23 {- so arbitrary -} bs

get_forecast_ipgeo :: IO Location
get_forecast_ipgeo = do
    json <- withManager defaultManagerSettings trick_forecast_io
    return . either error unforecastio $ eitherDecode' json

trick_forecast_io mgr = do
    req <- parseUrl "http://forecast.io/ipgeo"
    let trickedreq = req { requestHeaders =
                            [("Referer", "https://forecast.io/")] }
    fmap responseBody $ httpLbs trickedreq mgr

newtype ForecastioLoc = ForecastioLoc { unforecastio :: Location } deriving Show

instance FromJSON ForecastioLoc where
    parseJSON (Object obj) = do
        rv <- Location <$> obj .: "name" <*> obj .: "latitude"
                                         <*> obj .: "longitude"
        return $ ForecastioLoc rv
    parseJSON _ = mzero

$(deriveFromJSON defaultOptions ''Forecast)
