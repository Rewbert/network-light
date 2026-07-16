# network-light

[![Hackage](https://img.shields.io/hackage/v/network-light.svg)](https://hackage.haskell.org/package/network-light)
[![Apache 2.0 License](<https://img.shields.io/badge/license-Apache%202.0-blue.svg>)](LICENSE)

A small, portable subset of the `network` package's socket API.

## What this is

`network-light` implements a small subset of the socket functionality found in the `network` package: creating TCP/UDP sockets addressable via IPv4, connecting, binding, listening, accepting connections, and sending/receiving raw bytes, `String`, or `ByteString`.
The API deliberately mirrors `network`'s in spirit, so moving between the two should feel familiar.

It exists because [MicroHs](https://github.com/augustss/MicroHs) (`mhs`), a small Haskell compiler, cannot yet compile the full `network` package. `network-light` is implemented directly on top of the C `socket()`/`connect()`/`send()`/`recv()`/... calls via plain FFI imports (no C stubs) which keeps it simple enough to compile under both GHC and MicroHs from the same source. Until MicroHs can compile `network` in full, this package is the quickest way to get sockets working under both compilers.

**This is not, and does not try to be, a replacement for `network`.** It only implements what has been needed so far — `Domain` and `SockOpt`, for example, each model a handful of constructors, not the full POSIX surface.

## Scope

- TCP (`SOCK_STREAM`) and UDP (`SOCK_DGRAM`) sockets over IPv4 (`AF_INET`)
- `connect`, `bind`, `listen`, `accept`, `close`
- Sending and receiving raw buffers, `String`, or `ByteString`, either "best effort"   or looped until the full amount is sent/received
- A handful of socket options: `SO_REUSEADDR`, `SO_DEBUG`, `SO_TYPE`, and   non-blocking mode
- Sockets are non-blocking by default and integrate correctly with both GHC's I/O manager and MicroHs's cooperative, green-thread concurrency
- Compiles under GHC and MicroHs, on Linux; a `zephyr` cabal flag selects the differing `sockaddr_in` layout needed for Zephyr RTOS embedded targets

If you need something this package doesn't have yet — IPv6, more socket options, Unix domain sockets, and so on — please fork it, add what you need, and open a pull request. Contributions are very welcome, as long as they keep to the existing style: plain FFI imports, no C stubs unless truly unavoidable, and code that compiles under both GHC and MicroHs.

## Installation

```
cabal install network-light
```

or add it to your `.cabal` file:

```
build-depends: network-light
```

## Example

```haskell
module Main where

import System.Network

port :: Int
port = 4242

server :: IO ()
server = do
    serverFd <- socket AF_INET SOCK_STREAM
    setsocketopt serverFd SO_REUSEADDR 1
    bind serverFd (mkSockAddr port Nothing)
    listen serverFd 1

    (clientFd, clientAddr) <- accept serverFd
    putStrLn ("received connection from: " <> show clientAddr)

    msg <- recvString clientFd 100
    putStrLn msg
    _ <- sendString clientFd "Hello, client!"

    close clientFd
    close serverFd

client :: IO ()
client = do
    fd <- socket AF_INET SOCK_STREAM
    connect fd (mkSockAddr port (Just "127.0.0.1"))

    _ <- sendString fd "Hello, server!"
    reply <- recvString fd 100
    putStrLn reply

    close fd

-- Run `server` in one terminal and `client` in another; they will talk to each other.
main :: IO ()
main = server
```

## Testing

The test suite lives under `tests/` and is driven by `make` rather than
`cabal test`, so that every test is built and run against both GHC and MicroHs from the same source:

```
cd tests
make test          # run every test under both mhs and ghc
make test HC=ghc   # GHC only
make test HC=mhs   # MicroHs only
```

Tested with GHC 9.10.3, 9.12.2, and 9.14.1 (see `tested-with` in the `.cabal` file), and with MicroHs 0.16.4.0.

## License

Apache License 2.0. See [LICENSE](LICENSE).
