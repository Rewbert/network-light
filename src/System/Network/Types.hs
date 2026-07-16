{-# LANGUAGE CPP #-}
-- | Shared types, instances, and pure helpers used by both
-- "Network.Blocking" and "Network.NonBlocking".
--
-- Two values here describe the platform @struct sockaddr_in@ ABI and differ
-- between Linux and Zephyr.  They are selected with the @ZEPHYR@ CPP macro,
-- which the Zephyr build passes via @mhs -XCPP -DZEPHYR@ (plain @__MHS__@ is
-- defined on both targets and so cannot tell them apart):
--
--   * @AF_INET@: 2 on Linux, 1 (@NET_AF_INET@) on Zephyr.
--   * @sizeof(struct sockaddr_in)@: 16 on Linux (8 + @sin_zero@), 8 on Zephyr
--     (@net_sockaddr_in@ has no padding).
--
-- The field order and endianness are identical on both, so 'peekSockAddr' is
-- platform-neutral.
module System.Network.Types
    ( Socket(..)
    , Domain(..)
    , StreamType(..)
    , SockAddr(..)
    , SockOpt(..)
    , mkSockAddr
    , cSizeOf
    , cFromEnum
    ) where

import Data.Bits
import Data.List
import Data.Word

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | A socket file descriptor.
newtype Socket = Socket CInt
  deriving (Eq, Ord, Show)

-- | The domain models addressing families. Only IPv4 is support right now, but more can easily be added.
data Domain
    = AF_INET -- ^ IPv4

instance Enum Domain where
#if defined(ZEPHYR)
    toEnum 1 = AF_INET
    toEnum _ = error "Domain.toEnum: unrecognised value"
    fromEnum AF_INET = 1
#else
    toEnum 2 = AF_INET
    toEnum _ = error "Domain.toEnum: unrecognised value"
    fromEnum AF_INET = 2
#endif

-- | The stream type used by a socket
data StreamType
    = SOCK_STREAM -- ^ TCP
    | SOCK_DGRAM  -- ^ UDP

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum 2 = SOCK_DGRAM
    toEnum _ = error "StreamType.toEnum: unrecognised value"
    fromEnum SOCK_STREAM = 1
    fromEnum SOCK_DGRAM  = 2

{- | Socket address.  Internals are intentionally opaque to callers.
Create values of this type via 'mkSockAddr'.-}
data SockAddr = SockAddrInet Int String deriving (Show, Read, Ord, Eq)

{- | Construct a 'SockAddr' from a port and an address. E.g. to create a socket to
target localhost, post 3232, you call @mkSockAddr 3232 "127.0.0.1"@.

Pass 'Nothing' for the address to get @INADDR_ANY@ (@0.0.0.0@). -}
mkSockAddr :: Int -> Maybe String -> SockAddr
mkSockAddr port (Just address) = SockAddrInet port address
mkSockAddr port Nothing        = SockAddrInet port "0.0.0.0"

-- | Socket options for use with @setsocketopt@.
data SockOpt
    = SO_REUSEADDR -- ^ Allow binding to a local address left in @TIME_WAIT@
                   --   by a previous socket, so a restarted listener does not
                   --   fail with @EADDRINUSE@.
    | SO_DEBUG     -- ^ Enable kernel-level debug tracing for the socket.
    | SO_TYPE      -- ^ The socket's type (e.g. @SOCK_STREAM@ vs
                   --   @SOCK_DGRAM@). Note: this is a get-only option on
                   --   POSIX; passing it to 'setsocketopt' is not meaningful
                   --   and will typically fail.
    | O_NONBLOCK   -- ^ Put the socket into non-blocking mode. Not a
                   --   @SOL_SOCKET@ option: 'setsocketopt' special-cases it,
                   --   applying it via @fcntl@\/@F_SETFL@ on the MHS runtime
                   --   and treating it as a no-op under GHC, whose IO manager
                   --   already runs sockets non-blocking.

-- ---------------------------------------------------------------------------
-- Storable SockAddr
-- ---------------------------------------------------------------------------

instance Storable SockAddr where
#if defined(ZEPHYR)
    sizeOf    _ = 8
#else
    sizeOf    _ = 16
#endif
    alignment _ = 16
    peek        = peekSockAddr
    poke        = pokeSockAddr

-- | Serialise a 'SockAddr' into a @struct sockaddr_in@ laid out in memory.
-- Only 'AF_INET' is supported; the @sin_family@ field is set per platform
-- (see the module header for the @ZEPHYR@ macro).
pokeSockAddr :: Ptr SockAddr -> SockAddr -> IO ()
pokeSockAddr p (SockAddrInet port address) =
    pokeArray (castPtr p) (sin_family ++ sin_port ++ sin_addr)
  where
    sin_family :: [Word8]
#if defined(ZEPHYR)
    sin_family = [0x01, 0x00]
#else
    sin_family = [0x02, 0x00]
#endif

    sin_port :: [Word8]
    sin_port =
        let high = fromIntegral ((port `shiftR` 8) .&. 0xFF)
            low  = fromIntegral  (port             .&. 0xFF)
        in [high, low]

    sin_addr :: [Word8]
    sin_addr = take 4 $ map read $ splitOn '.' address
      where
        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn sep xs =
            let pref = takeWhile (/= sep) xs
                suff = dropWhile (/= sep) xs
            in case suff of
                 []     -> [pref]
                 (_:t)  -> pref : splitOn sep t

-- | Deserialise a @struct sockaddr_in@ from memory into a 'SockAddr'.
-- Reads the port from bytes 2–3 (network byte order) and the IPv4 address
-- from bytes 4–7.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
    xs <- peekArray 8 (castPtr p :: Ptr Word8)
    case xs of
        _:_:high:low:sin_addr ->
            let port    = (fromIntegral high `shiftL` 8) .|. fromIntegral low
                address = intercalate "." $ map show sin_addr
            in  return $ SockAddrInet port address
        _ -> error "peekSockAddr: unexpected buffer layout"

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

-- | Return the 'sizeOf' of a value as a 'CInt', for passing to C functions.
cSizeOf :: Storable a => a -> CInt
cSizeOf = CInt . fromIntegral . sizeOf

-- | Convert a Haskell 'Enum' value to a 'CInt', for passing to C functions.
cFromEnum :: Enum a => a -> CInt
cFromEnum = CInt . fromIntegral . fromEnum
