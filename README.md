This package, `network-light`, implements a small subset of the functionality in the `network` package.
You can write code that uses TCP/UDP sockets, adressable via IPV4, and send or read data over them. Some socket options can be configured, but please contribute and add more options if you need one that is not added yet.

It is meant to be small, low on dependencies, self contained, and most importantly, easily compilable by MicroHaskell.

Here is an example client/server.

```haskell
module Main where

import Network.Network

client :: IO ()
client = do
    sockfd <- socket AF_INET SOCK_STREAM
    let server_addr = mkSockAddr 4242 (Just "127.0.0.1")
    
    connect sockfd server_addr
    
    _ <- sendString sockfd "Hello, Server!"
    s <- recvString sockfd 100
    putStrLn s
    
    close sockfd

server :: IO ()
server = do
    server_fd <- socket AF_INET SOCK_STREAM
    let server_addr = mkSockAddr 4242 Nothing
    
    bind server_fd server_addr
    listen server_fd
    (client_fd, client_addr) <- accept server_fd
    putStrLn $ "received connection request from: " <> show client_addr
    
    putStrLn =<< recvString client_fd 100
    _ <- sendString client_fd "Hello, client!"
    
    close client_fd
    close server_fd

-- replace with client and compile again, and execute the two binaries in different terminals.
-- they will communicate with each other.
main :: IO ()
main = client
```
