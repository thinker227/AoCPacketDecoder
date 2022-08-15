{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-} -- Do-notation is more explicit and readable

module PacketDecoder where

import HexMagic
import BitMagic
import Control.Monad.State



data Packet = Packet { version :: Int,
                       typeId :: Int,
                       payload :: Payload }
                       deriving (Show, Eq)
data Payload
    = Literal { value :: Int }
    | Operator { subpackets :: [Packet] } deriving (Show, Eq)
type PacketData = [Bool]



readBit :: State PacketData Bool
readBit = state f
    where f packet = (head packet, tail packet)

readBits :: Int -> State PacketData [Bool]
readBits length = state f
    where f packet = splitAt length packet

readInt :: Int -> State PacketData Int
readInt length = state f
    where f packet = (getIntFromBits $ take length packet, drop length packet)

readLiteralBits :: State PacketData [Bool]
readLiteralBits = do
    leading <- readBit
    bits <- readBits 3

    if leading
    then do
        next <- readLiteralBits
        pure $ bits ++ next
    else pure bits

readLiteral :: State PacketData Payload
readLiteral = do
    bits <- readLiteralBits
    pure $ Literal $ getIntFromBits bits

readOperator :: State PacketData Payload
readOperator = error "Not implemented"

readPayload :: Int -> State PacketData Payload
readPayload typeId = do
    if typeId == 4
    then readLiteral
    else readOperator

readPacket :: State PacketData Packet
readPacket = do
    version <- readInt 3
    typeId <- readInt 3
    payload <- readPayload typeId
    let result = Packet version typeId payload
    pure result

createPacket :: [Bool] -> Packet
createPacket bits = do
    let (packet, _) = runState readPacket bits
    packet

foo :: IO ()
foo = putStrLn "Many people have this power!"
