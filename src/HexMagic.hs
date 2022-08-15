module HexMagic (
    hexCharToInt,
    getHexCharBits,
    getHexBits
) where

import Data.Char ( toLower )
import Data.Bits ( Bits((.&.), shiftR) )
import BitMagic ( getNybbleBits )

hexCharToInt :: Char -> Int
hexCharToInt hex =
    case toLower hex of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _ -> 0

getHexCharBits :: Char -> [Bool]
getHexCharBits = getNybbleBits . hexCharToInt

getHexBits :: String -> [Bool]
getHexBits = concatMap getHexCharBits
