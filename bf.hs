{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad.State (StateT, get, put, modify, when, runStateT)
import Control.Monad.Trans (liftIO)
import Data.Word (Word8)
import Data.Char (chr, ord)
import Text.Parsec (char, many, between, choice, parse, spaces)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import Data.Typeable (Typeable)
import Data.Data (Data)

data Machine
    = Machine
    { cells :: [(Int, Word8)]
    , pointer :: Int
    }
    deriving Show

type BF = StateT Machine IO

data Instr
    = Advance
    | Backstep
    | Inc
    | Dec
    | PutChar
    | GetChar
    | While [Instr]
    deriving (Data, Typeable, Show)

alter :: Eq a => [(a, b)] -> (b -> b) -> a -> b -> [(a, b)]
alter [] _ k v = [(k, v)]
alter ((kalt, valt):xs) f k v
    | k == kalt = (k, f valt) : xs
    | otherwise = (k, valt) : alter xs f k v

eval :: Instr -> BF ()
eval Advance = modify $ \m -> m { pointer = pointer m + 1 }
eval Backstep = modify $ \m -> m { pointer = pointer m - 1 }
eval Inc = modify $ \(Machine c p) -> Machine (alter c (+1) p 1) p
eval Dec = modify $ \(Machine c p) -> Machine (alter c (subtract 1) p 0xff) p
eval PutChar = get >>= liftIO . putchar
    where putchar (Machine c p) = print . chr . fromIntegral . maybe 0 id
                                $ lookup p c
eval GetChar = do
    u8 <- liftIO $ fmap (fromIntegral . ord) getChar
    modify $ \(Machine c p) -> Machine (alter c (const u8) p u8) p
eval (While instrs) = loop
    where
    loop = do
        z <- get >>= \(Machine c p) -> return . maybe 0 id $ lookup p c
        when (z /= 0) $ mapM_ eval instrs >> loop

prev = char '<' >> return Backstep
next = char '>' >> return Advance
inc = char '+' >> return Inc
dec = char '-' >> return Dec
pput = char '.' >> return PutChar
gget = char ',' >> return GetChar
while = fmap While $ between (char '[') (char ']') aprog
aprog = spaces >> many (choice [prev, next, inc, dec, pput, gget, while])

bf :: String -> [Instr]
bf src = case parse aprog "" src of
    Right bfexpr -> bfexpr
    Left buzzkill -> error $ show buzzkill

k = QuasiQuoter
    { quoteExp = dataToExpQ (const Nothing) . bf
    , quotePat = const $ error "no patterns"
    , quoteType = const $ error "no types"
    , quoteDec = const $ error "no decls"
    }
