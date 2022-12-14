module Main where

import System.Environment ( getArgs )
import HexMagic ( getHexBits )
import BitMagic ( bitsToBinaryString )
import PacketDecoder ( createPacket, sumVersions, evaluatePacket )

main :: IO ()
main = do
    envArgs <- getArgs
    input <- readFile $ head envArgs
    putStrLn $ "Input: " ++ input
    
    let bits = getHexBits input
    putStrLn $ "Bits: " ++ bitsToBinaryString bits
    let packet = createPacket bits
    putStrLn $ "Root packet: " ++ show packet

    let versionSum = sumVersions packet
    putStrLn $ "Part 1 (version sum): " ++ show versionSum

    let evalResult = evaluatePacket packet
    putStrLn $ "Part 2 (evaluated expression): " ++ show evalResult
