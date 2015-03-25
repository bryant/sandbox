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
import Numeric (readSigned, readFloat, showFFloat)
import System.Environment (getArgs)
import Prelude hiding (take, drop)

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> getit False
    ["-si"] -> getit True
    _ -> putStrLn usage

getit si = do
    loc <- get_forecast_ipgeo
    wetter <- mein_wetter si bry_creds loc
    pretty_print si loc wetter

mein_wetter :: Bool -> ForecastCreds -> Location -> IO Forecast
mein_wetter si anmelden wo = do
    json <- simpleHttp $ forecast_uri si_ anmelden wo
    return . either error currently $ eitherDecode' json
    where si_ = if si then "?si" else ""

usage = "Usage: wetter [-si] [-h]"

pretty_print :: Bool -> Location -> Forecast -> IO ()
pretty_print si (Location n lat lon) wetter = do
    putStr $ unlines
        [ unwords [ "Weather for"
                  , n
                  , "(" ++ coord lat ++ ","
                  , coord lat ++ "):"
                  ]
        , unwords [ summary wetter
                  , show $ icon wetter
                  , deg $ temperature wetter]
        , unwords ["Feels like", deg $ apparentTemperature wetter]
        ]
    where
    deg val = showFFloat (Just 1) val $ '\x00b0' : if si then "C" else "F"
    coord val = showFFloat (Just 2) val "\x00b0"

forecast_uri si anmelden wo =
    "https://api.forecast.io/forecast/" ++ anmelden ++ "/"
                                        ++ show (loc_lat wo) ++ ","
                                        ++ show (loc_lon wo) ++ si

data Current a = Current { currently :: a } deriving Show

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
    deriving Show

data Icon = ClearDay | ClearNight | Rain | Snow | Sleet | Wind | Fog | Cloudy
          | PartlyCloudyDay | PartlyCloudyNight | Nada

instance Show Icon where
    show x = case x of
        ClearDay -> "\x263c"
        ClearNight -> "\x263d"
        Rain -> "\x2614"
        Snow -> "\x2744"
        Sleet -> "sleet"
        Wind -> "wind"  -- todo
        Fog -> "\x1f32b"  -- todo
        Cloudy -> "\x2601"
        PartlyCloudyDay -> "\x2601"  -- todo
        PartlyCloudyNight ->"\x2601"  -- todo
        _ -> ""

instance FromJSON Icon where
    parseJSON v = return $ case v of
        "clear-day" -> ClearDay
        "clear-night" -> ClearNight
        "rain" -> Rain
        "snow" -> Snow
        "sleet" -> Sleet
        "wind" -> Wind
        "fog" -> Fog
        "cloudy" -> Cloudy
        "partly-cloudy-day" -> PartlyCloudyDay
        "partly-cloudy-night" -> PartlyCloudyNight
        _ -> Nada

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

$(deriveFromJSON defaultOptions ''Current)
$(deriveFromJSON defaultOptions ''Forecast)
