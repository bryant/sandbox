{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as Char8

import Network.HTTP.Conduit (simpleHttp, httpLbs)
import Network.HTTP.Client (withManager, defaultManagerSettings, parseUrl,
                            requestHeaders, responseBody)
import Data.ByteString.Lazy.Search (indices)
import Data.ByteString.Lazy.Char8 (unpack, take, drop)
import Text.JSON (JSON(..), JSValue(..), Result(..), fromJSString,
                  fromJSObject, decode, valFromObj)
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
    , loc_lat :: Float
    , loc_lon :: Float
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
    case decode $ unpack json of
         Ok (ForecastioLoc loc) -> return loc
         Error str -> error str

trick_forecast_io mgr = do
    req <- parseUrl "http://forecast.io/ipgeo"
    let trickedreq = req { requestHeaders =
                            [("Referer", "https://forecast.io/")] }
    fmap responseBody $ httpLbs trickedreq mgr

newtype ForecastioLoc = ForecastioLoc Location deriving Show

instance JSON ForecastioLoc where
    showJSON = undefined  -- don't need it
    readJSON (JSObject obj) = do
        lat <- valFromObj "latitude" obj
        lon <- valFromObj "longitude" obj
        city <- valFromObj "name" obj
        return . ForecastioLoc $ Location city lat lon
