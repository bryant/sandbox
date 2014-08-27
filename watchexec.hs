import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.Cmd (system)
import System.Environment (getArgs)
import qualified System.INotify as Inot

-- from github.com/guard/guard/issues/98
events :: [Inot.EventVariety]
events = [Inot.MoveSelf, Inot.CloseWrite]

when_modified files cmd = Inot.withINotify $ \inot -> do
    bracket (add_watches inot) remove_watches (const sleep_forever)
    where
    exec = system cmd >> return ()
    add_watches i = mapM (\f -> Inot.addWatch i events f $ const exec) files
    remove_watches = mapM Inot.removeWatch
    sleep_forever = threadDelay one_sec >> sleep_forever
    one_sec = 1000000

usage :: String
usage = unlines
    [ "Usage: watchexec file [file ...] cmd"
    , "watchexec will watch one or more files for common text editor save"
    , "events and execute a command upon the occurence of such an event."
    ]

main :: IO ()
main = do
    args <- getArgs
    case reverse args of
        cmd : file : files -> (file:files) `when_modified` cmd
        _ -> putStrLn usage
