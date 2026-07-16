module Server where

import System.Environment
import System.IO
import System.Network

port :: Int
port = 9932

serve :: Socket -> IO ()
serve sfd = do
    (cfd, _) <- accept sfd
    msg <- recvString cfd 1024
    putStrLn ("SERVER_GOT:" ++ msg)
    hFlush stdout
    sendString cfd ("ack:" ++ msg)
    close cfd

main :: IO ()
main = do
    [nStr] <- getArgs
    let n = read nStr :: Int

    sfd <- socket AF_INET SOCK_STREAM
    setsocketopt sfd SO_REUSEADDR 1
    bind sfd (mkSockAddr port Nothing)
    listen sfd n

    putStrLn "READY"
    hFlush stdout

    -- Clients connect concurrently (the OS queues them in the listen
    -- backlog); the server itself just drains that backlog one at a
    -- time, which is enough to prove several clients can be in flight
    -- against the same listening socket at once.
    mapM_ (const (serve sfd)) [1 .. n]

    close sfd
    putStrLn "SERVER_DONE"
    hFlush stdout
