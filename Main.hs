
module Main
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Char
import Data.Word
import Data.Bits
import System.IO

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  bsIn <- C.getContents
  let hexBs = C.filter (isHexDigit) bsIn
  B.putStr $ B.pack $ unhex hexBs

unhex :: C.ByteString -> [Word8]
unhex bsIn =
  let (nibble, rest) = C.splitAt 2 bsIn
  in case (C.unpack nibble) of
     hi:lo:[] -> (unhexByte hi lo) : unhex rest
     _        -> []

unhexByte :: Char -> Char -> Word8
unhexByte hi lo =
  shift (unhexNibble hi) 4 .|. unhexNibble lo

unhexNibble :: Char -> Word8
unhexNibble ch =
  let (chDiff, intDiff) = diff ch
  in toEnum $ (fromEnum ch) - (fromEnum chDiff) + intDiff

diff :: Char -> (Char, Int)
diff ch | ch >= 'a' = ('a', 10)
        | ch >= 'A' = ('A', 10)
        | ch >= '0' = ('0',  0)
        | otherwise = error $ "Unexpected char [" ++ (show ch) ++ "] in unhexNibble"
