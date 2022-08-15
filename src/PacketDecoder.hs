{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-} -- Do-notation is more explicit and readable

module PacketDecoder(
    Packet,
    Payload,
    createPacket,
    sumVersions,
    evaluatePacket
) where

import BitMagic ( boolToInteger, getIntFromBits )
import Control.Monad.State ( State, runState, MonadState(get, state) )



data Packet = Packet { version :: Int,
                       typeId :: Int,
                       payload :: Payload }
                       deriving (Show, Eq)
data Payload
    = Literal { value :: Int }
    | Operator { typeLengthId :: Bool,
                 lengthOrCount :: Int,
                 subpackets :: [Packet] }
                 deriving (Show, Eq)
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
    bits <- readBits 4

    if leading
    then do
        next <- readLiteralBits
        pure $ bits ++ next
    else pure bits

readLiteral :: State PacketData Payload
readLiteral = do
    bits <- readLiteralBits
    pure $ Literal $ getIntFromBits bits

readSubpacketsFromSize' :: Int -> State PacketData [Packet]
readSubpacketsFromSize' targetSize = do
    packet <- readPacket

    currentData <- get
    let currentSize = length currentData
    if currentSize > targetSize then do
        next <- readSubpacketsFromSize' targetSize
        pure $ packet : next
    else do
        pure [packet]

readSubpacketsFromSize :: Int -> State PacketData [Packet]
readSubpacketsFromSize size = do
    currentData <- get
    let currentSize = length currentData
    let targetSize = currentSize - size
    readSubpacketsFromSize' targetSize

readSubpacketsFromCount :: Int -> State PacketData [Packet]
readSubpacketsFromCount count = do
    packet <- readPacket
    if count > 1 then do
        next <- readSubpacketsFromCount (count - 1)
        pure $ packet : next
    else pure [packet]

readOperator :: State PacketData Payload
readOperator = do
    lengthTypeId <- readBit
    let bitCount = if lengthTypeId
        then 11
        else 15
    lengthOrCount <- readInt bitCount
    subpackets <- if lengthTypeId
        then readSubpacketsFromCount lengthOrCount
        else readSubpacketsFromSize lengthOrCount
    pure $ Operator lengthTypeId lengthOrCount subpackets

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

getSubpackets :: Packet -> [Packet]
getSubpackets packet =
    case payload packet of
        Literal _ -> []
        Operator _ _ subpackets -> subpackets

getAllSubpackets :: Packet -> [Packet]
getAllSubpackets root =
    let subs = getSubpackets root in
    subs ++ concatMap getAllSubpackets subs

sumVersions :: Packet -> Int
sumVersions root = sum $ map version $ root : getAllSubpackets root

getLiteral :: Packet -> Int
getLiteral packet = case payload packet of
    Literal n -> n
    _ -> 0

evaluatePacket :: Packet -> Integer
evaluatePacket packet = let
    subs = map evaluatePacket $ subpackets $ payload packet
    (first:second:_) = subs       
    in case typeId packet of
        0 -> sum subs
        1 -> product subs
        2 -> minimum subs
        3 -> maximum subs
        4 -> toInteger $ getLiteral packet
        5 -> boolToInteger $ first > second
        6 -> boolToInteger $ first < second
        7 -> boolToInteger $ first == second
        _ -> error "Invalid type ID"
