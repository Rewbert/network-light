module Server where

import System.Environment
import System.IO
import System.Network

port :: Int
port = 9933

-- One churn cycle: bind a fresh socket to the same port, accept exactly
-- one connection, and actively close it from our end (the side that
-- closes first is the side that lands in TIME_WAIT). Without
-- SO_REUSEADDR the next iteration's bind fails with EADDRINUSE while
-- that TIME_WAIT entry is still around.
churn :: Int -> IO ()
churn i = do
    sfd <- socket AF_INET SOCK_STREAM
    setsocketopt sfd SO_REUSEADDR 1
    bind sfd (mkSockAddr port Nothing)
    listen sfd 1

    putStrLn ("READY_" ++ show i)
    hFlush stdout

    (afd, _) <- accept sfd
    close afd
    close sfd

main :: IO ()
main = do
    [nStr] <- getArgs
    mapM_ churn [1 .. (read nStr :: Int)]
    putStrLn "SERVER_DONE"
    hFlush stdout
