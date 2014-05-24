{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Control.Applicative ((<*>), (<$>), (*>))
import System.IO (Handle, IOMode(..), BufferMode(..), hSetBuffering, hClose)
import Network.Socket

import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Attoparsec.ByteString.Char8 as Atto

type Key = BS.ByteString
type Value = BS.ByteString
type RetVal = BS.ByteString
data Command = Get Key | Set Key Value deriving Show
type Store = Map.Map Key Value
data AppState = AppState { store :: Store }
type AppT = State.StateT AppState

empty_store = AppState Map.empty :: AppState

exec :: (Functor m, Monad m) => Command -> AppT m RetVal
exec (Set k v) = do
    State.modify $ \st -> st { store = Map.insert k v $ store st }
    return "set: ()"
exec (Get k) = do
    st <- fmap store State.get
    return $ case Map.lookup k st of
        Nothing -> "()"
        Just x -> x

field = Atto.space >> Atto.takeWhile1 notSpace
    where notSpace = not . Atto.isSpace
pget = Get <$> (Atto.string "get" *> field)
pset = Set <$> (Atto.string "set" *> field) <*> field

parse_cmd :: BS.ByteString -> Either String Command
parse_cmd = Atto.parseOnly $ Atto.choice [pset, pget]

listen_on :: PortNumber -> IO Socket
listen_on port = do
    lsock <- socket AF_INET Stream 0
    setSocketOption lsock ReuseAddr 1
    bindSocket lsock $ SockAddrInet port iNADDR_ANY
    listen lsock 2
    return lsock

accept_handle :: Socket -> IO (Handle, SockAddr)
accept_handle sock = do
    (csock, addr) <- accept sock
    handle <- socketToHandle csock ReadWriteMode
    hSetBuffering handle NoBuffering
    return (handle, addr)

accept_loop :: Socket -> AppT IO ()
accept_loop lsock = forever $ do
    (handle, _) <- lift $ accept_handle lsock
    cmd <- lift $ BS.hGetLine handle
    rv <- case parse_cmd cmd of
        Right c -> exec c
        Left err -> return $ BS.pack err
    lift $ BS.hPutStr handle (BS.append rv "\n") >> hClose handle

main = withSocketsDo $ do
    putStrLn $ "Listening on port " ++ show port
    lsock <- listen_on port
    flip State.runStateT empty_store $ accept_loop lsock
    sClose lsock
    where port = 8000
