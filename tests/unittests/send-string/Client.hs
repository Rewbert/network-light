module Client where

import System.IO
import System.Network

port :: Int
port = 9931

message :: String
message = "hello-network-light"

main :: IO ()
main = do
    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))

    sendString fd message
    reply <- recvString fd 1024
    putStrLn ("CLIENT_GOT:" ++ reply)
    hFlush stdout

    close fd
