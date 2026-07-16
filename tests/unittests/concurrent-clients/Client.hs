module Client where

import System.Environment
import System.IO
import System.Network

port :: Int
port = 9932

main :: IO ()
main = do
    [idx] <- getArgs
    let msg = "client-" ++ idx

    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))

    sendString fd msg
    reply <- recvString fd 1024
    putStrLn ("CLIENT_" ++ idx ++ "_GOT:" ++ reply)
    hFlush stdout

    close fd
