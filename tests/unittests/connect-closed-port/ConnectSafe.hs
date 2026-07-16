module ConnectSafe where

import System.Network

-- Nothing listens here; connect' should return False instead of throwing.
port :: Int
port = 9934

main :: IO ()
main = do
    fd <- socket AF_INET SOCK_STREAM
    ok <- connect' fd (mkSockAddr port (Just "127.0.0.1"))
    putStrLn ("CONNECT_RESULT:" ++ show ok)
