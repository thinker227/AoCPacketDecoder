module Main where

import HexMagic (getHexBits)
import BitMagic (bitsToBinaryString)
import PacketDecoder
import System.Environment (getArgs)

main :: IO ()
main = do
    envArgs <- getArgs
    input <- readFile $ head envArgs
    putStrLn $ "Input: " ++ input
    
    let bits = getHexBits input
    putStrLn $ "Bits: " ++ bitsToBinaryString bits
    let packet = createPacket bits
    putStrLn $ "Root packet: " ++ show packet
