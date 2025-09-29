{-| Simple Socket interface, that only supports AF_INET so far. There are other addressing families (e.g. BT)
that are not supported (because I am lazy).

Most functions only exist as IO () variants, where in reality they might fail with an error code.
These variants can be trivially added.

-}
{-# LANGUAGE CPP #-}
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

#if defined(__GLASGOW_HASKELL__)
type CSSize = CInt
#endif

foreign import ccall "sys/socket.h socket"  c_socket        :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h connect" c_connect       :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind"    c_bind          :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept"  c_accept        :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall "sys/socket.h listen"  c_listen        :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h send"    c_send          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize
foreign import ccall "sys/socket.h recv"    c_recv          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize
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
    toEnum _ = error "not added yet"

    fromEnum AF_INET = 2

-- | Stream type
data StreamType
    = SOCK_STREAM -- ^ TCP
    | SOCK_DGRAM  -- ^ UDP
    -- many more to add

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum 2 = SOCK_DGRAM
    toEnum _ = error "not added yet"

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
    toEnum _ = error "not added yet"

    fromEnum SO_REUSEADDR = 2

-- | Create a new socket, throwing an exception if creation failed
socket :: Domain -> StreamType -> IO Socket
socket d st = do
    i <- throwErrnoIfMinus1 "error allocating socket" $
             c_socket (CInt $ fromIntegral $ fromEnum d) (CInt $ fromIntegral $ fromEnum st) 0
    return (Socket i)

-- | Set a socket option
setsocketopt :: Socket -> SockOpt -> IO ()
setsocketopt (Socket socketfd) so = do
    -- please see the C setsocketopt to see what the parameters are, if it is unclear
    -- (it was unclear to me first)
    with (1 :: Word8) $ \p -> do
        _ <- throwErrnoIfMinus1 "error in setsocketopt" $ do
                c_setsockopt socketfd (CInt 1 {- SOL_SOCKET = 1 -}) (CInt $ fromIntegral $ fromEnum so) p (CInt $ fromIntegral (sizeOf (undefined :: CInt)))
        return ()

-- | Close a socket
close :: Socket -> IO ()
close (Socket fd@(CInt fd')) = do
    _ <- throwErrnoIfMinus1 ("error closing socket " ++ show fd') $ do
            c_close fd
    return ()

-- | Establish a connection
connect :: Socket -> SockAddr -> IO ()
connect (Socket socketfd) sockaddr =
    with sockaddr $ \p -> do
        _ <- throwErrnoIfMinus1 "error in connect" $ do
                c_connect socketfd p (CInt $ fromIntegral $ sizeOf sockaddr)
        return ()

-- | Establish a connection, returning a Bool to indicate whether the action was successful
connect' :: Socket -> SockAddr -> IO Bool
connect' (Socket socketfd) sockaddr =
    with sockaddr $ \p -> do
        CInt e <- c_connect socketfd p (CInt $ fromIntegral $ sizeOf sockaddr)
        return $ e >= 0

-- | Bind a socket to a SockAddr
bind :: Socket -> SockAddr -> IO ()
bind (Socket socketfd) sockaddr =
    with sockaddr $ \p -> do
        _ <- throwErrnoIfNegative "error in bind" $ do
                c_bind socketfd p (CInt (fromIntegral $ sizeOf sockaddr))
        return ()

-- | Accept an incoming connection request
accept :: Socket -> IO (Socket, SockAddr)
accept (Socket server_fd) = do
    allocaBytes (sizeOf (undefined :: SockAddr)) $ \p ->
        allocaBytes 4 $ \p_size -> do
            poke p_size 16

            CInt e <- throwErrnoIfNegative "error in accept" $
                c_accept server_fd p p_size
            sockaddr <- peekSockAddr p
            return (Socket (CInt e), sockaddr)

-- | Set a socket to listening mode
listen :: Socket -> IO ()
listen (Socket df) = do
    _ <- throwErrnoIfNegative "error in listen" $
            c_listen df (CInt 1)
    return ()

-- | Send a buffer of data over a socket
sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf (Socket socketfd) buf len =
    throwErrnoIfMinus1 "error in send" $ do
#if defined(__GLASGOW_HASKELL__)
        CInt e <-
#else
        e <-
#endif
            c_send socketfd buf (CSize (fromIntegral len)) (CInt 0)
        return $ fromIntegral e

-- | Convenience function to transmit a String over a socket
sendString :: Socket -> String -> IO Int
sendString sock str =
    let bytes = map (fromIntegral . ord) str :: [Word8]
    in withArray bytes $ \ptr ->
        sendBuf sock ptr (length bytes)

-- | Receive data over a socket
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf (Socket socketfd) buf len = do
    throwErrnoIfMinus1 "error in recv" $ do
#if defined(__GLASGOW_HASKELL__)
        CInt e <-
#else
        e <-
#endif
            c_recv socketfd buf (CSize (fromIntegral len)) (CInt 0)
        return $ fromIntegral e

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

callocaBytes :: Int -> (Ptr a -> IO b) -> IO b
callocaBytes s f = do
    p <- callocBytes s
    b <- f p
    free p
    return b

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
        let w16 = fromIntegral port :: Word16
            high = fromIntegral ((w16 `shiftR` 8) .&. 0xFF)
            low = fromIntegral (w16 .&. 0xFF)
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
peekSockAddr p =
    let p' :: Ptr Word8
        p' = castPtr p
    in do
        xs <- peekArray 8 p'
        case xs of
            _:_:high:low:sin_addr -> do
                let w16     =  (fromIntegral high `shiftL` 8) .|. fromIntegral low :: Word16
                    port    = fromIntegral w16 :: Int
                    address = intercalate "." $ map show sin_addr
                return $ SockAddrInet port address
            _ -> error "somehow wrong format lmao"

throwErrnoIfNegative :: (Ord a, Num a) => String -> IO a -> IO a
throwErrnoIfNegative str act = do
    r <- act
    if r < 0
        then error str
        else return r