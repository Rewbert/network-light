module Server where

import Data.Char
import System.IO
import System.Network

port :: Int
port = 9935

main :: IO ()
main = do
    sfd <- socket AF_INET SOCK_STREAM
    setsocketopt sfd SO_REUSEADDR 1
    bind sfd (mkSockAddr port Nothing)
    listen sfd 1

    putStrLn "READY"
    hFlush stdout

    (cfd, _) <- accept sfd
    msg <- recvString cfd 65536
    hPutStrLn stderr ("SERVER_GOT:" ++ show (length msg) ++ " bytes")
    sendString cfd (map toUpper msg)
    close cfd
    close sfd
