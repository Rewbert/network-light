module Client where

import System.IO
import System.Network

port :: Int
port = 9935

main :: IO ()
main = do
    input <- getContents

    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))

    sendString fd input
    reply <- recvString fd 65536
    close fd

    putStr reply
    hFlush stdout
