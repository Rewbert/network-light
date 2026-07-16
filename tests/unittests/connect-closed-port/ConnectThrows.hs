module ConnectThrows where

import System.Network

-- Nothing listens here; connecting should raise an IO exception.
port :: Int
port = 9934

main :: IO ()
main = do
    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))
    putStrLn "UNEXPECTED_SUCCESS"
