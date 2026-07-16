module Server where

import System.IO
import System.Network

port :: Int
port = 9931

main :: IO ()
main = do
    sfd <- socket AF_INET SOCK_STREAM
    setsocketopt sfd SO_REUSEADDR 1
    bind sfd (mkSockAddr port Nothing)
    listen sfd 1

    -- Tell the test harness we're ready to accept a connection.
    putStrLn "READY"
    hFlush stdout

    (cfd, _) <- accept sfd
    msg <- recvString cfd 1024
    putStrLn ("SERVER_GOT:" ++ msg)
    hFlush stdout

    sendString cfd ("ack:" ++ msg)
    close cfd
    close sfd
