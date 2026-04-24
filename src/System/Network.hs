{-# LANGUAGE CPP #-}
{- | Non-blocking socket interface.

Under mhs, sockets are set to O_NONBLOCK at creation time and all
blocking operations yield the green thread (via 'System.IO.FD') instead
of blocking the OS thread.

Under GHC, the runtime's I/O manager provides equivalent behaviour
transparently, so this module falls back to ordinary blocking FFI calls. -}
module System.Network
    ( -- * Re-exports from "Network.Types"
      module System.Network.Types
      -- * Operations
    , socket
    , setsocketopt
    , close
    , connect
    , connect'
    , bind
    , accept
    , listen
    , sendBuf
    , sendString
    , recvBuf
    , recvString
    ) where

import Data.Word

#ifdef __MHS__
import System.IO.FD (waitForReadFD, waitForWriteFD)
#endif

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import System.Network.Types

-- ---------------------------------------------------------------------------
-- FFI
-- ---------------------------------------------------------------------------

foreign import ccall "sys/socket.h socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h connect" c_connect :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind" c_bind :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept" c_accept :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall "sys/socket.h listen" c_listen :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h send" c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall "sys/socket.h recv" c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall "sys/socket.h setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "unistd.h close" c_close :: CInt -> IO CInt

#ifdef __MHS__
foreign import ccall "fcntl.h fcntl" c_fcntl :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h getsockopt" c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

f_SETFL :: CInt
f_SETFL    = 4     -- F_SETFL

o_NONBLOCK :: CInt
o_NONBLOCK = 2048  -- O_NONBLOCK (0x800, Linux), wonderful magic number

fdInt :: CInt -> Int
fdInt = fromIntegral

-- | Read SO_ERROR after a non-blocking connect completes.
peekSockError :: CInt -> IO CInt
peekSockError fd =
    alloca $ \errPtr -> alloca $ \lenPtr -> do
        poke lenPtr (cSizeOf (0 :: CInt))
        _ <- c_getsockopt fd 1 4 errPtr lenPtr
        peek errPtr
#endif

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Create a new socket.
socket :: Domain -> StreamType -> IO Socket
socket d st = do
    fd <- throwErrnoIfMinus1 "socket" $
              c_socket (cFromEnum d) (cFromEnum st) 0
#ifdef __MHS__
    let sock = Socket fd
    setsocketopt (Socket fd) O_NONBLOCK (error "do not evaluate")
#endif
    return (Socket fd)

-- | Set a socket option.
setsocketopt :: Socket -> SockOpt -> Int -> IO ()
#ifdef __MHS__
setsocketopt (Socket fd) O_NONBLOCK _ =
    throwErrnoIfMinus1_ "setsocketopt/O_NONBLOCK" $
        c_fcntl fd f_SETFL o_NONBLOCK
#else
setsocketopt _ O_NONBLOCK _ = return () -- sockets created via GHC already have this setting from the IO manager?
#endif
setsocketopt (Socket fd) so value =
    with (fromIntegral value :: Word8) $ \p ->
        throwErrnoIfMinus1_ "setsocketopt" $
            let (option, level) = case so of
                  SO_REUSEADDR -> (CInt 2, CInt 1)
                  SO_DEBUG     -> (CInt 1, CInt 1)
                  SO_TYPE      -> (CInt 3, CInt 1)
            in c_setsockopt fd level option p (cSizeOf (0 :: CInt))

-- | Close a socket.
close :: Socket -> IO ()
close (Socket fd@(CInt n)) =
    throwErrnoIfMinus1_ ("close socket " ++ show n) $
        c_close fd

-- | Connect.  Throws on failure.
connect :: Socket -> SockAddr -> IO ()
connect (Socket fd) sockaddr =
#ifdef __MHS__
    with sockaddr $ \p -> do
        r <- c_connect fd p (cSizeOf sockaddr)
        if r /= -1 then return () else do
            errno <- getErrno
            if errno == eINPROGRESS
              then do waitForWriteFD (fdInt fd)
                      err <- peekSockError fd
                      if err /= 0 then throwErrno "connect" else return ()
              else throwErrno "connect"
#else
    with sockaddr $ \p ->
        throwErrnoIfMinus1_ "connect" $
            c_connect fd p (cSizeOf sockaddr)
#endif

-- | Connect, returning 'False' instead of throwing on failure.
connect' :: Socket -> SockAddr -> IO Bool
connect' (Socket fd) sockaddr =
#ifdef __MHS__
    with sockaddr $ \p -> do
        r <- c_connect fd p (cSizeOf sockaddr)
        if r /= -1 then return True else do
            errno <- getErrno
            if errno == eINPROGRESS
              then do waitForWriteFD (fdInt fd)
                      err <- peekSockError fd
                      return (err == 0)
              else return False
#else
    with sockaddr $ \p -> do
        CInt e <- c_connect fd p (cSizeOf sockaddr)
        return (e >= 0)
#endif

-- | Bind a socket to an address.
bind :: Socket -> SockAddr -> IO ()
bind (Socket fd) sockaddr =
    with sockaddr $ \p ->
        throwErrnoIfMinus1_ "bind" $
            c_bind fd p (cSizeOf sockaddr)

-- | Accept an incoming connection.
accept :: Socket -> IO (Socket, SockAddr)
accept (Socket serverFd) =
#ifdef __MHS__
    allocaBytes (sizeOf (undefined :: SockAddr)) $ \p ->
        with (cSizeOf (undefined :: SockAddr)) $ \pSize ->
            go p pSize
  where
    go p pSize = do
        r <- c_accept serverFd p pSize
        if r /= -1
          then do addr <- peek p
                  throwErrnoIfMinus1_ "accept/setnonblock" $
                      c_fcntl r f_SETFL o_NONBLOCK
                  return (Socket r, addr)
          else do errno <- getErrno
                  if errno == eAGAIN || errno == eWOULDBLOCK
                    then waitForReadFD (fdInt serverFd) >> go p pSize
                    else throwErrno "accept"
#else
    allocaBytes (sizeOf (undefined :: SockAddr)) $ \p ->
        with (cSizeOf (undefined :: SockAddr)) $ \pSize -> do
            clientFd <- throwErrnoIfMinus1 "accept" $
                            c_accept serverFd p pSize
            addr <- peek p
            return (Socket clientFd, addr)
#endif

-- | Set the socket to listening mode.
listen :: Socket -> IO ()
listen (Socket fd) =
    throwErrnoIfMinus1_ "listen" $
        c_listen fd (CInt 1)

-- | Send raw bytes.  Returns the number of bytes actually sent.
sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
#ifdef __MHS__
sendBuf (Socket fd) buf len = go
  where
    go = do
        CInt n <- c_send fd buf (CSize (fromIntegral len)) (CInt 0)
        if n /= -1 then return (fromIntegral n) else do
            errno <- getErrno
            if errno == eAGAIN || errno == eWOULDBLOCK
              then waitForWriteFD (fdInt fd) >> go
              else throwErrno "sendBuf"
#else
sendBuf (Socket fd) buf len =
    throwErrnoIfMinus1 "sendBuf" $ do
        CInt n <- c_send fd buf (CSize (fromIntegral len)) (CInt 0)
        return (fromIntegral n)
#endif

-- | Send a 'String' over a socket.
sendString :: Socket -> String -> IO Int
sendString sock str =
    withCAStringLen str $ \(ptr, len) ->
        sendBuf sock (castPtr ptr) len

-- | Receive raw bytes into an existing buffer.  Returns byte count.
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
#ifdef __MHS__
recvBuf (Socket fd) buf len = go
  where
    go = do
        CInt n <- c_recv fd buf (CSize (fromIntegral len)) (CInt 0)
        if n /= -1 then return (fromIntegral n) else do
            errno <- getErrno
            if errno == eAGAIN || errno == eWOULDBLOCK
              then waitForReadFD (fdInt fd) >> go
              else throwErrno "recvBuf"
#else
recvBuf (Socket fd) buf len =
    throwErrnoIfMinus1 "recvBuf" $ do
        CInt n <- c_recv fd buf (CSize (fromIntegral len)) (CInt 0)
        return (fromIntegral n)
#endif

-- | Receive up to @maxLen@ bytes and decode as a 'String'.
recvString :: Socket -> Int -> IO String
recvString sock maxLen =
    allocaBytes maxLen $ \buf -> do
        n <- recvBuf sock buf maxLen
        peekCAStringLen (castPtr buf, n)
