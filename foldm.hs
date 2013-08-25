import Control.Monad
import Data.Maybe

x ./. 0 = Nothing
x ./. y = Just (x / y)
