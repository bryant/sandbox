{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Attoparsec.ByteString.Lazy as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Attoc

import Network.HTTP.Conduit (simpleHttp, httpLbs)
import Network.HTTP.Client (withManager, defaultManagerSettings, parseUrl,
                            requestHeaders, responseBody)
import Data.ByteString.Lazy.Search (indices, breakAfter)
import Data.ByteString.Lazy.Char8 (unpack, take, drop)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.Aeson (FromJSON(parseJSON), eitherDecode', (.:), Value(Object))
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Numeric (readSigned, readFloat, showFFloat)
import System.Environment (getArgs, getProgName)
import System.Random (getStdRandom, randomR)
import Prelude hiding (take, drop)

main :: IO ()
main = do
    nem <- getProgName
    let si = if head nem == 's' then True else False
    args <- getArgs
    case args of
        [] -> get_forecast_ipgeo >>= getit si
        [location] -> gmaps_addr_to_ll location >>= \loc -> case loc of
            Nothing -> putStrLn $ "Couldn't get lat-long for " ++ location
            Just loc -> getit si loc
        _ -> putStrLn usage

getit si loc = do
    wetter <- mein_wetter si bry_creds loc
    pretty_print si loc wetter

mein_wetter :: Bool -> ForecastCreds -> Location -> IO Forecast
mein_wetter si anmelden wo = do
    json <- simpleHttp $ forecast_uri si_ anmelden wo
    return . either error currently $ eitherDecode' json
    where si_ = if si then "?si" else ""

usage = "Usage: [s]wetter [location]"

pretty_print :: Bool -> Location -> Forecast -> IO ()
pretty_print si (Location n lat lon) wetter = do
    prop <- propaganda
    putStr $ unlines
        [ unwords [ "Weather for"
                  , n
                  , "(" ++ coord lat ++ ","
                  , coord lon ++ "):"
                  ]
        , unwords [ summary wetter
                  , show $ icon wetter
                  , deg $ temperature wetter
                  , "feels like", deg $ apparentTemperature wetter]
        , unwords ["**", prop, "**"]
        ]
    where
    deg val = showFFloat (Just 1) val $ '\x00b0' : if si then "C" else "F"
    coord val = showFFloat (Just 2) val "\x00b0"

propaganda = fmap (p !!) (getStdRandom $ randomR (0, length p - 1))
    where
    p = [ "Wetter is true love. Use wetter."
        , "What's the weather? Get wetter! Use wetter."
        , "Tom wanted a third message, so here's a new one: Use wetter!"
        , "Eat döner. Use wetter."
        ]

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

gmaps_addr_to_ll :: String -> IO (Maybe Location)
gmaps_addr_to_ll term =
        find_coords . snd . breakAfter "spotlight" <$> simpleHttp url
    where
    url = "https://www.google.com/maps?q=," ++ term

    find_coords "" = Nothing
    find_coords xs = case Atto.parse namecoord xs of
        Atto.Fail _ _ _ -> find_coords $ drop 1 xs
        Atto.Done _ rv -> Just rv

    namecoord = do
        Attoc.char '\"'
        loc <- Attoc.takeWhile1 (/= '\"')
        Attoc.char '\"'
        _ <- Attoc.string ",null,[null,null,"
        lat <- Attoc.double
        Attoc.char ','
        long <- Attoc.double
        return $ Location (Char8.unpack loc) lat long

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
