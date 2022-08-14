module Main where

import PacketDecoder
import System.Environment (getArgs)

main :: IO ()
main = do
    envArgs <- getArgs
    input <- readFile $ head envArgs
    putStrLn $ "Input: " ++ input
    
    foo
