module Server where

import System.Network

port :: Int
port = 9936

main :: IO ()
main = do
    sfd <- socket AF_INET SOCK_STREAM
    setsocketopt sfd SO_REUSEADDR 1
    bind sfd (mkSockAddr port Nothing)
    listen sfd 1

    (cfd, _) <- accept sfd
    msg <- recvString cfd 1024
    putStrLn msg

    close cfd
    close sfd
