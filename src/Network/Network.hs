{-| Simple Socket interface, that only supports AF_INET so far. There are other addressing families (e.g. BT)
that are not supported (because I am lazy).

Most functions only exist as IO () variants, where in reality they might fail with an error code.
These variants can be trivially added.

-}
module Network.Network where

import Data.Char
import Data.Word
import Data.Bits
import Data.List

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.Error

foreign import ccall "sys/socket.h socket"  c_socket        :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h connect" c_connect       :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind"    c_bind          :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept"  c_accept        :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall "sys/socket.h listen"  c_listen        :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h send"    c_send          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall "sys/socket.h recv"    c_recv          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall "sys/socket.h setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "unistd.h     close"   c_close   :: CInt -> IO CInt

-- | Socket type
newtype Socket = Socket CInt

-- | Adressing family
data Domain
    = AF_INET -- ^ IPV4
    -- many more that are not added

instance Enum Domain where
    toEnum 2 = AF_INET
    toEnum _ = error "Domain.toEnum unimplemented"

    fromEnum AF_INET = 2

-- | Stream type
data StreamType
    = SOCK_STREAM -- ^ TCP
    | SOCK_DGRAM  -- ^ UDP
    -- many more to add

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum 2 = SOCK_DGRAM
    toEnum _ = error "StreamType.toEnum unimplemented"

    fromEnum SOCK_STREAM = 1
    fromEnum SOCK_DGRAM = 2

-- | Socket Address type, don't expose internals of this one
data SockAddr = SockAddrInet Int String deriving (Show, Eq)

{- | Create a SockAddr. If the second parameter is Nothing, the address is assumed
to be INADDR_ANY (0.0.0.0) -}
mkSockAddr :: Int -> Maybe String -> SockAddr
mkSockAddr port (Just address) = SockAddrInet port address
mkSockAddr port Nothing        = SockAddrInet port "0.0.0.0" -- INADDR_ANY

-- | Socket options
data SockOpt
    = SO_REUSEADDR -- ^ Allow reuse of local addresses
    -- and many more to add, I just need this one for my specific application

instance Enum SockOpt where
    toEnum 2 = SO_REUSEADDR
    toEnum _ = error "SockOpt.toEnum unimplemented"

    fromEnum SO_REUSEADDR = 2

-- | Create a new socket, throwing an exception if creation failed
socket :: Domain -> StreamType -> IO Socket
socket d st = do
    i <- throwErrnoIfMinus1 "socket" $
             c_socket (cFromEnum d) (cFromEnum st) 0
    return (Socket i)

-- | Set a socket option
setsocketopt :: Socket -> SockOpt -> IO ()
setsocketopt (Socket socketfd) so = do
    -- please see the C setsocketopt to see what the parameters are, if it is unclear
    -- (it was unclear to me first)
    with (1 :: Word8) $ \p -> do
        throwErrnoIfMinus1_ "setsocketopt" $
            c_setsockopt socketfd (CInt 1 {- SOL_SOCKET = 1 -}) (cFromEnum so) p (cSizeOf (0 :: CInt))

-- | Close a socket
close :: Socket -> IO ()
close (Socket fd@(CInt fd')) = do
    throwErrnoIfMinus1_ ("close socket " ++ show fd') $ do
        c_close fd

-- | Establish a connection
connect :: Socket -> SockAddr -> IO ()
connect (Socket socketfd) sockaddr =
    with sockaddr $ \p ->
        throwErrnoIfMinus1_ "connect" $ do
            c_connect socketfd p (cSizeOf sockaddr)

-- | Establish a connection, returning a Bool to indicate whether the action was successful
connect' :: Socket -> SockAddr -> IO Bool
connect' (Socket socketfd) sockaddr =
    with sockaddr $ \p -> do
        CInt e <- c_connect socketfd p (cSizeOf sockaddr)
        return $ e >= 0

-- | Bind a socket to a SockAddr
bind :: Socket -> SockAddr -> IO ()
bind (Socket socketfd) sockaddr =
    with sockaddr $ \p -> do
        throwErrnoIfMinus1_ "bind" $ do
            c_bind socketfd p (cSizeOf sockaddr)

-- | Accept an incoming connection request
accept :: Socket -> IO (Socket, SockAddr)
accept (Socket server_fd) = do
    allocaBytes (sizeOf (undefined :: SockAddr)) $ \p ->
        with (cSizeOf (undefined :: SockAddr)) $ \p_size -> do
            s <- throwErrnoIfMinus1 "accept" $
                     c_accept server_fd p p_size
            sockaddr <- peekSockAddr p
            return (Socket s, sockaddr)

-- | Set a socket to listening mode
listen :: Socket -> IO ()
listen (Socket df) = do
    throwErrnoIfMinus1_ "listen" $
        c_listen df (CInt 1)

-- | Send a buffer of data over a socket
sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf (Socket socketfd) buf len =
    throwErrnoIfMinus1 "send" $ do
        CInt e <- c_send socketfd buf (CSize $ fromIntegral len) (CInt 0)
        return e

-- | Convenience function to transmit a String over a socket
sendString :: Socket -> String -> IO Int
sendString sock str =
    let bytes = map (fromIntegral . ord) str :: [Word8]
    in withArray bytes $ \ptr ->
        sendBuf sock ptr (length bytes)

-- | Receive data over a socket
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf (Socket socketfd) buf len = do
    throwErrnoIfMinus1 "recv" $ do
        CInt e <- c_recv socketfd buf (CSize $ fromIntegral len) (CInt 0)
        return e

-- | Convenience function to receive a string (of a maximum length)
recvString :: Socket -> Int -> IO String
recvString sock len = allocaBytes len $ \buf -> do
    n <- recvBuf sock buf len
    bytes <- mapM (\i -> peekByteOff buf i :: IO Word8) [0 .. (n-1)]
    return $ map (chr . fromIntegral) bytes

instance Storable SockAddr where
    sizeOf _ = 16
    alignment _ = 16 -- ?
    peek = peekSockAddr
    poke = pokeSockAddr

-- | Write a SockAddr to a buffer
pokeSockAddr :: Ptr SockAddr -> SockAddr -> IO ()
pokeSockAddr p (SockAddrInet port address) =
    pokeArray (castPtr p) $ (sin_family ++ sin_port ++ sin_addr)
  where
    -- hard-coded AF_INET for now
    sin_family :: [Word8]
    sin_family = [0x02, 0x00]

    -- the port in network order
    sin_port :: [Word8]
    sin_port =
        let high = fromIntegral ((port `shiftR` 8) .&. 0xFF)
            low  = fromIntegral (port .&. 0xFF)
        in [high, low]
    
    -- the address (e.g. "127.0.0.1" -> [127,0,0,1] :: [Word8])
    sin_addr :: [Word8]
    sin_addr = map read $ splitOn '.' address
      where
        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn i xs = let pref = takeWhile ((/=) i) xs
                           suff = dropWhile ((/=) i) xs
                       in case suff of
                            [] -> [pref]
                            (_:xs') -> pref : splitOn i xs'

-- | Read a SockAddr from a buffer
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
    xs <- peekArray 8 (castPtr p :: Ptr Word8)
    case xs of
        _:_:high:low:sin_addr -> do
            let port    = (fromIntegral high `shiftL` 8) .|. fromIntegral low
                address = intercalate "." $ map show sin_addr
            return $ SockAddrInet port address
        _ -> undefined

cSizeOf :: (Storable a) => a -> CInt
cSizeOf = CInt . sizeOf

cFromEnum :: (Enum a) => a -> CInt
cFromEnum = CInt . fromEnum
