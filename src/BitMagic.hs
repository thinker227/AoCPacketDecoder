module BitMagic (
    bitIs1,
    boolToBit,
    getNybbleBits,
    getIntFromBits,
    bitsToBinaryString,
    boolToInt,
    boolToInteger
) where

import Data.Bits

bitIs1 bit num = ((num `shiftR` bit) .&. 1) == 1

boolToBit :: Bool -> Int
boolToBit b = if b then 1 else 0

getNybbleBits num =
    [ bitIs1 3 num,
      bitIs1 2 num,
      bitIs1 1 num,
      bitIs1 0 num ]

getIntFromBits' :: [Bool] -> Int -> Int
getIntFromBits' [bit] num = num .|. boolToBit bit
getIntFromBits' (bit:bits) num =
    getIntFromBits' bits $ (num .|. boolToBit bit) `shiftL` 1
getIntFromBits' _ _ = error "No more bits"

getIntFromBits :: [Bool] -> Int
getIntFromBits bits = getIntFromBits' bits 0

bitsToBinaryString :: [Bool] -> String
bitsToBinaryString = map (\b -> if b then '1' else '0')

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

boolToInteger :: Bool -> Integer
boolToInteger = toInteger . boolToInt
