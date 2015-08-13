import Control.Applicative ((<$>))
import Data.Serialize (Get, runGet, getWord8)

data D = A | B | C deriving (Show, Eq, Enum, Bounded)

get_enum :: (Enum a, Bounded a, Integral b) => Get b -> Get (Maybe a)
get_enum getter = safe_enum . fromIntegral <$> getter

safe_enum :: (Enum a, Bounded a) => Int -> Maybe a
safe_enum = _safe_enum minBound maxBound

_safe_enum :: (Enum a, Bounded a) => a -> a -> Int -> Maybe a
_safe_enum lower upper val
    | fromEnum lower <= val && val <= fromEnum upper = Just $ toEnum val
    | otherwise = Nothing
