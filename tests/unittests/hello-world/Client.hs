module Client where

import System.Network

port :: Int
port = 9936

main :: IO ()
main = do
    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))

    sendString fd "hello world"

    close fd
