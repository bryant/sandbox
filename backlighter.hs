import System.Posix.User (setUserID)
import System.Environment (getArgs, getProgName)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (string, munch1, choice, ReadP, readP_to_S,
                                     eof)

readBrightness :: String -> Maybe Int
readBrightness preset = case readP_to_S presetParser preset of
    [] -> Nothing
    (bright, _) : _ -> Just bright
    where
    presetParser = choice
                 [ opt "dim" >> return 16
                 , opt "low" >> return 32
                 , opt "med" >> return 64
                 , opt "hi" >> return 128
                 , read `fmap` munch1 isDigit
                 ]
    opt xs = string xs >> eof

backlightFile = "/sys/class/backlight/intel_backlight/brightness"

main :: IO ()
main = do
    arg <- getArgs
    case arg of
        [] -> showUsage
        n : _ -> maybe showUsage adjust $ readBrightness n
    where
    showUsage = do
        p <- getProgName
        putStrLn $ "Usage: " ++ p ++ " (n|dim|low|med|hi)"
    adjust n = setUserID 0 >> writeFile backlightFile n'
                           >> putStrLn ("Adjusted to " ++ n')
        where n' = show n
