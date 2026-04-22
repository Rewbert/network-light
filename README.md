This package, `network-light`, implements a small subset of the functionality in the `network` package.
You can write code that uses TCP/UDP sockets, adressable via IPV4, and send or read data over them. Some socket options can be configured, but please contribute and add more options if you need one that is not added yet.

**note**: This package implements the functions and data type variants that I've needed for my purposes, so does not yet fully match the `network` package on Hackage (if that was the case, this package would not be needed). Please fork and add the functionality you need, and open PRs. I'll happily merge them. Please keep it simple and follow the existing style (foreign imports, no cbits unless absolutely necessary, verify that it compiles with both ghc and mhs).

It is meant to be small, low on dependencies, self contained, and most importantly, easily compilable by MicroHaskell. Please contribute!

Here is an example client/server.

```haskell
module Main where

import System.Network

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

-- replace with server and compile again, and execute the two binaries in different terminals.
-- they will communicate with each other.
main :: IO ()
main = client
```
