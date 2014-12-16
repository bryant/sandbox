{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module SaltedShuffle where

import Data.Word
import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeIndex)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BStr

s :: UCharArray
s = listArray (0, BStr.length s' - 1) $ BStr.unpack s'
    where s' = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

type UCharArray = UArray Int Word8

salted_shuffle :: UCharArray -> UCharArray -> UCharArray
salted_shuffle input salt = runST $
    thaw input >>= loop (len input - 1) (fromIntegral $ salt ! 0) 0 >>= freeze
    where
    len = snd . bounds

    loop :: Int -> Int -> Int -> (STUArray s) Int Word8
         -> ST s ((STUArray s) Int Word8)
    loop !ind !summ !grainpos arr = if ind < 1 then return arr else swap
        where
        swap = do
            k <- unsafeRead arr ind
            unsafeRead arr alt >>= unsafeWrite arr ind >> unsafeWrite arr alt k
            loop (ind - 1) (summ + grain') grainpos' arr
        grainpos' = (grainpos + 1) `rem` len salt
        grain' = fromIntegral $ salt ! grainpos'
        grain = fromIntegral $ salt ! grainpos
        alt = (summ + grainpos + grain) `rem` ind

--shuffleN :: ByteString -> Int -> ByteString
shuffleN !b !0 = b
shuffleN !b !n = shuffleN (salted_shuffle b b) (n-1)

main :: IO ()
main = print . BStr.pack . elems $ shuffleN s 2000000
--main = print . head . drop 2000000 . iterate (\ss -> salted_shuffle ss ss) $ s
