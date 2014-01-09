import Control.Monad.State (State(..), get, put, runState)

newtype Context = Context {
    blocks :: [Int]
    } deriving Show

type MyState = State Context

serpent :: MyState ()
serpent = do
    Context oldlist <- get
    put . Context $ 6 : oldlist
